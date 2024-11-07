data Shape = Point
            |Circle Float -- constructor names
            |Rectangle Float Float
            |Square Float
            |Triangle Float Float
            |Composed Shape Shape

instance Show Shape where
    show Point = "."
    show (Circle r) = "o Circle with radius: " ++ show r
    show (Rectangle l w) = "[] Rectangle with length: " ++ show l ++ " and width: " ++ show w
    show (Square s) = "[ ] Square with side: " ++ show s
    show (Triangle b h) = "^ Triangle with base: " ++ show b ++ " and height: " ++ show h
    show (Composed s1 s2) = "Shape 1: " ++ show s1 ++ " Shape 2: " ++ show s2


area :: Shape -> Float
area Point = 0
area(Circle r) = pi * r^2 -- r*r also works
area(Rectangle l w) = l*w
area(Square s) = s^2
area(Triangle b h) = 1/2 * b * h
area (Composed s1 s2) = area s1 + area s2 -- area(s1) would also work

s1 :: Shape
s1 = Point

s2 = Circle 5.3

s3 = Rectangle 10 12.4

s4 = Square 5.7

s5 = Triangle 4 10.2

s6 = Composed s3 s5

areaC :: Shape -> Float -- using case statement
areaC s = case s of 
            Point -> 0
            Circle r -> pi * r^2
            Rectangle l w -> l*w
            Square s -> s^2
            Triangle b h -> 0.5 * b * h
            Composed s1 s2 -> areaC s1 + areaC s2

getShape :: IO Shape
getShape = do
            putStrLn "Choose a shape: (point, circle, rectangle, square, triangle, composed)"
            shapeType <- getLine -- getLine will return string
            case shapeType of
                "point" -> return Point
                "circle" -> do
                                putStrLn "Enter a radius:"
                                radius <- readLn -- readLn will return float
                                return (Circle radius)
                "rectangle" -> do
                                putStrLn "Enter a length:"
                                length <- readLn
                                putStrLn "Enter a width:"
                                width <- readLn
                                return (Rectangle length width)
                "square" -> do
                                putStrLn "Enter a side:"
                                side <- readLn
                                return (Square side)
                "triangle" -> do
                                putStrLn "Enter a base:"
                                base <- readLn
                                putStrLn "Enter a height:"
                                height <- readLn
                                return (Triangle base height)
                "composed" -> do
                                putStrLn "Shape 1: "
                                s1 <- getShape
                                putStrLn "Shape 2: "
                                s2 <- getShape
                                return (Composed s1 s2)

main :: IO()
main = do
        s <- getShape
        let a = area s
        print a