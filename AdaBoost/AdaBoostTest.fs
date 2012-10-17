module AdaBoost.Test

type PointType = {
    X : int
    Y : int
}

type CircleType = {
    Center : PointType
    Radius : int
} with
    static member IsPointInner point circle =
        let distanceSquare { X = x1; Y =  y1 }  { X = x2; Y = y2 } = pown (x1 - x2) 2 + pown (y1 - y2) 2
        distanceSquare point circle.Center <= circle.Radius

type ShapeKind = Point of PointType | Circle of CircleType

open System.Drawing

type Shape = {
    Kind : ShapeKind
    Color : Color
} with
    member this.Draw (canvas : Graphics) =
        match this.Kind with
        | Point { X = x; Y = y } -> canvas.DrawRectangle(new Pen(new SolidBrush(this.Color)), x, y, 1, 1)
        | Circle { Center = { X = x; Y = y }; Radius = r } ->
            canvas.DrawEllipse(new Pen(new SolidBrush(this.Color)), x - r, y - r, r * 2 + 1, r * 2 + 1)

let draw (fileName : string) (width : int) (height : int) shapes =
    use bitmap = new Bitmap(width, height)
    use canvas = Graphics.FromImage bitmap
    shapes |> Seq.iter (fun (shape : Shape) -> shape.Draw canvas)
    bitmap.Save(fileName, Imaging.ImageFormat.Png)
