module AdaBoost.Test

let distanceSquare (x1, y1)  (x2, y2) = pown (x1 - x2) 2 + pown (y1 - y2) 2

type CircleType = {
    Center : int * int
    Radius : int
} with
    static member IsPointInner point circle = distanceSquare point circle.Center <= circle.Radius

type ShapeKind = Point of (int * int) | Circle of CircleType

open System.Drawing

type Shape = {
    Kind : ShapeKind
    Color : Color
} with
    member this.Draw (canvas : Graphics) =
        match this.Kind with
        | Point(x, y) -> canvas.DrawRectangle(new Pen(new SolidBrush(this.Color)), x, y, 1, 1)
        | Circle { Center = (x, y); Radius = r } ->
            canvas.DrawEllipse(new Pen(new SolidBrush(this.Color)), x - r, y - r, r * 2 + 1, r * 2 + 1)

let draw (fileName : string) (width : int) (height : int) shapes =
    use bitmap = new Bitmap(width, height)
    use canvas = Graphics.FromImage bitmap
    shapes |> Seq.iter (fun (shape : Shape) -> shape.Draw canvas)
    bitmap.Save(fileName, Imaging.ImageFormat.Png)

let getPointComparisons max = seq {
    for elementStr, elementFunc in ["x", fst; "y", snd] do
    for comparerStr, comparerFunc in ["<", (>); ">", (<)] do
    for c in 0 .. max ->
        { Name = sprintf "%s %s %d" elementStr comparerStr c; Classify = elementFunc >> comparerFunc c >> Label.FromBool } }

let getDistanceComparisons max points = seq {
    let points = points |> Seq.toArray
    for i in 0 .. points.Length - 1 do
    for r in 1 .. max ->
        { Name = sprintf "distance to points.[%d] < %d" i r; Classify = distanceSquare points.[i] >> (>) r >> Label.FromBool } }

let getCosines point1 point2 = seq {
    let (x1, y1), (x2, y2) = point1, point2
    let norm (x, y) = x * x + y * y |> float |> sqrt
    let getCosine (x, y) =
        let (x1', y1'), (x2', y2') = (x1 - x, y1 - y), (x2 - x, y2 - y)
        float (x1' * x2' + y1' * y2') / (norm (x1', y1') * norm (x2', y2'))
    for t in -1. .. 0.01 .. 1. ->
        { Name = sprintf "cos θ ≒ %f" t; Classify = getCosine >> (fun cosθ -> t - 0.005 <= cosθ && cosθ < t + 0.005) >> Label.FromBool } }
