module AdaBoost

/// 正事例(1)または負事例(-1)のいずれかの値をとるラベル
type Label = private Label of int with
    static member Create = function 1 -> Label 1 | -1 -> Label -1 | _ -> invalidArg "value" "1または-1を指定してください。"
    static member CreateFromSign labelIfZero = function 0 -> labelIfZero | value -> Label.Create value
    static member ToInt = function Label x -> x
    static member IsPositive = function Label 1 -> true | _ -> false
let (|Label|) = Label.ToInt

/// 分類器
type Classifier<'example> = {
    /// 分類器の名前
    Name : string
    /// 事例からラベルを求める関数
    Classify : 'example -> Label
} with
    override this.ToString() = this.Name

open FSharp.Control

/// <summary>
/// 弱分類器と訓練データから仮説を生成する
/// </summary>
/// <param name="weakClassifiers">弱分類器のリスト</param>
/// <param name="trainingDatum">訓練データのリスト</param>
/// <return>(仮説, (仮説を構成する弱分類器, 重み)の重要な順から並べたリスト)</return>
let setUp weakClassifiers trainingDatum =
    /// 弱分類器のリストから最良の弱分類器とそのエラー率を抽出する
    let getBestClassifier trainingDatum distributions =
        /// 期待するラベルと弱分類器の結果のラベルが異なっている場合は1を、等しい場合は0を返す
        let isError (Label expectedLabel) (Label actualLabel) = (expectedLabel + 1) ^^^ (actualLabel + 1) >>> 1
        /// 弱分類器の重み付けされたエラー率を求める
        let getTotalError trainingDatum distributions classifier =
            Seq.zip trainingDatum distributions
            |> Seq.sumBy (fun ((data, label), distribution) -> data |> classifier.Classify |> isError label |> float |> (*) distribution)
        AsyncSeq.mapAsync (fun classifier -> async { return classifier, getTotalError trainingDatum distributions classifier })
        >> AsyncSeq.toBlockingSeq >> Seq.minBy snd

    /// 弱分類器のエラー率から、その弱分類器に対する重みを求める
    let getWeightOfClassifier error = (1.0 - error) / error |> log |> (*) 0.5

    /// 標本に対する重みの次のリストを求める
    let getNextDistributions classifier weight trainingDatum distributions =
        /// 正規化されていない重みの次のリスト
        let denormalizedNextDistributions =
            (AsyncSeq.ofSeq trainingDatum, AsyncSeq.ofSeq distributions) ||> AsyncSeq.zip
            |> AsyncSeq.mapAsync (fun ((data, Label label), distribution) ->
                async { return label * Label.ToInt (classifier.Classify data) |> float |> (*) -weight |> exp |> (*) distribution })
        /// 重みを正規化するための除数
        let normalizationFactor = denormalizedNextDistributions |> AsyncSeq.toBlockingSeq |> Seq.sum
        // 正規化された重みの次のリストを求める
        denormalizedNextDistributions |> AsyncSeq.mapAsync (fun nextDistribution -> async { return nextDistribution / normalizationFactor })
        |> AsyncSeq.toBlockingSeq
    
    /// 訓練データから、有効な弱分類器とその重みのリストを求める
    let chooseClassifiers trainingDatum classifiers =
        /// 訓練データの件数
        let trainingDatumCount = trainingDatum |> List.length
        /// 重みの初期リスト
        let initialDistributions = seq { for _ in 1 .. trainingDatumCount -> 1.0 / float trainingDatumCount }
        // エラー率の低い弱分類器を順に抽出したリストを作成する
        initialDistributions |> Seq.unfold (fun distributions ->
            match getBestClassifier trainingDatum distributions classifiers with
            | _, error when error >= 0.5 -> None
            | bestClassifier, error ->
                let weight = getWeightOfClassifier error
                let nextDistributions = getNextDistributions bestClassifier weight trainingDatum distributions
                Some((bestClassifier, weight), nextDistributions))
        |> Seq.toList

    /// 弱分類器の並列処理用シーケンス
    let weakClassifiers = weakClassifiers |> List.toSeq |> AsyncSeq.ofSeq

    /// 弱分類器と対応する重みのリスト
    let chosenClassifiers = chooseClassifiers trainingDatum weakClassifiers

    // 仮説
    let hypothesis queryData =
        chosenClassifiers |> List.sumBy (fun (classifier, weight) ->
            queryData |> classifier.Classify |> Label.ToInt |> float |> (*) weight)
        |> sign |> Label.CreateFromSign (Label.Create 1)

    hypothesis, chosenClassifiers
