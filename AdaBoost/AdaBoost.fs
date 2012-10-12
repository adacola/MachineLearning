module AdaBoost

/// 正事例(1)または負事例(-1)のいずれかの値をとるラベル
type Label = private Label of int with
    static member Create = function 1 -> Label 1 | -1 -> Label -1 | _ -> invalidArg "value" "1または-1を指定してください。"
    static member CreateFromSign labelIfZero = function 0 -> labelIfZero | value -> Label.Create value
    static member ToInt = function Label x -> x
let (|Label|) = Label.ToInt

/// 分類器
[<ReferenceEquality>]
type Hypothesis<'example> = {
    /// 事象からラベルを求める関数
    Evaluate : 'example -> Label
}

open FSharp.Control

/// 教師データと弱分類器のリストから仮説を生成し、検証データを評価する
let evaluate trainingDatum weakHypothesises =
    /// 教師データの件数
    let trainingDatumCount = trainingDatum |> List.length

    /// 弱分類器のリストから最良の弱分類器とそのエラー率を抽出する
    let getBestHypothesis trainingDatum distributions =
        let getError trainingDatum distributions hypothesis =
            Seq.zip trainingDatum distributions
            |> Seq.sumBy (fun ((data, Label label), distribution) ->
                (Label.ToInt (hypothesis.Evaluate data) - label) / 2 |> abs |> float |> (*) distribution)
        AsyncSeq.ofSeq >> AsyncSeq.mapAsync (fun hypothesis -> async { return hypothesis, getError trainingDatum distributions hypothesis })
        >> AsyncSeq.toBlockingSeq >> Seq.minBy snd

    /// 弱分類器のエラー率から、その弱分類器に対する重みを求める
    let getWeightOfHypothesis error = (1.0 - error) / error |> log |> (*) 0.5

    /// 標本に対する重みの次の値を求める
    let getNextDistributions hypothesis weight trainingDatum distributions =
        let denormalizedNextDistributions =
            (AsyncSeq.ofSeq trainingDatum, AsyncSeq.ofSeq distributions) ||> AsyncSeq.zip
            |> AsyncSeq.mapAsync (fun ((data, Label label), distribution) ->
                async { return label * Label.ToInt (hypothesis.Evaluate data) |> float |> (*) -weight |> exp |> (*) distribution })
        let normalizationFactor = denormalizedNextDistributions |> AsyncSeq.toBlockingSeq |> Seq.sum
        denormalizedNextDistributions |> AsyncSeq.mapAsync (fun nextDistribution -> async { return nextDistribution / normalizationFactor })
        |> AsyncSeq.toBlockingSeq
    
    /// 訓練データから、有効な弱分類器とその重みのリストを求める
    let chooseHypothesises trainingDatum hypothesises =
        let initialDistributions = seq { for _ in 1 .. trainingDatumCount -> 1.0 / float trainingDatumCount }
        (initialDistributions, hypothesises) |> Seq.unfold (fun (distributions, hypothesises) ->
            match getBestHypothesis trainingDatum distributions hypothesises with
            | _, error when error >= 0.5 -> None
            | bestHypothesis, error ->
                let weight = getWeightOfHypothesis error
                let nextDistributions = getNextDistributions bestHypothesis weight trainingDatum distributions
                let nextHypothesises = hypothesises |> Seq.filter ((<>) bestHypothesis)
                Some((bestHypothesis, weight), (nextDistributions, nextHypothesises)))
        |> Seq.toList

    // 仮説を返す
    fun queryData ->
        chooseHypothesises trainingDatum weakHypothesises |> List.sumBy (fun (hypothesis, weight) ->
            hypothesis.Evaluate queryData |> Label.ToInt |> float |> (*) weight)
        |> sign |> Label.CreateFromSign (Label.Create 1)
