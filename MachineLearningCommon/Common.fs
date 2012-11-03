module MachineLearning.Core

/// 正事例(1)または負事例(-1)のいずれかの値をとるラベル
type Label = private Label of int with
    static member Create = function 1 -> Label 1 | -1 -> Label -1 | _ -> invalidArg "value" "1または-1を指定してください。"
    static member CreateFromSign labelIfZero = function 0 -> labelIfZero | value -> Label.Create value
    static member ToInt = function Label x -> x
    static member IsPositive = function Label 1 -> true | _ -> false
    static member FromBool = function true -> Label 1 | false -> Label -1
let (|Label|) = Label.ToInt

/// 分類器
type Classifier<'example> = {
    /// 分類器の名前
    Name : string
    /// 事例からラベルを求める関数
    Classify : 'example -> Label
} with
    override this.ToString() = this.Name
