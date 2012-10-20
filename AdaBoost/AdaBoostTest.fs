module AdaBoost.Test

let distanceSquare (x1, y1)  (x2, y2) = pown (x1 - x2) 2 + pown (y1 - y2) 2

type Circle = {
    Center : int * int
    Radius : int
} with
    static member IsPointInner point circle = distanceSquare point circle.Center <= circle.Radius

type ShapeKind = Point of (int * int) | Circle of Circle

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

type Example = {
    Point : int * int
    DistancesToCircle : float[]
    Cosine : float
} with
    static member GetPoint { Point = p } = p
    static member Create circle1 circle2 ((x, y) as point) =
        let distances = [|circle1; circle2|] |> Array.map (fun { Center = center } ->  point |> distanceSquare center |> float |> sqrt)
        let cosine =
            let inline norm (x, y) = x * x + y * y |> float |> sqrt
            let (x1, y1), (x2, y2) = circle1.Center, circle2.Center
            let (x1', y1'), (x2', y2') = (x1 - x, y1 - y), (x2 - x, y2 - y)
            float (x1' * x2' + y1' * y2') / (norm (x1', y1') * norm (x2', y2'))
        { Point = point; DistancesToCircle = distances; Cosine = cosine }

let getPointClassifiers max = seq {
    for elementStr, elementFunc in ["x", Example.GetPoint >> fst; "y", Example.GetPoint >> snd] do
    for comparerStr, comparerFunc in ["<", (>); ">", (<)] do
    for c in 0 .. max ->
        { Name = sprintf "%s %s %d" elementStr comparerStr c
          Classify = elementFunc >> comparerFunc c >> Label.FromBool } }

let getDistanceClassifiers max = seq {
    for i in 0 .. 1 do
    for r in 1 .. max ->
        { Name = sprintf "distance to points.[%d] < %d" i r
          Classify = fun { DistancesToCircle = d } -> d.[i] < float r |> Label.FromBool } }

let getCosineClassifiers point1 point2 = seq {
    for t in -1. .. 0.01 .. 1. ->
        { Name = sprintf "cos θ < %f" t
          Classify = fun { Cosine = c } -> c < t |> Label.FromBool } }
