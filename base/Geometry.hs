module HaskellGame.Geometry where

data Shape = Rectangle { rectX :: Int, rectY :: Int, rectW :: Int, rectH :: Int } | Line { start :: (Int,Int), end :: (Int,Int) }
	| Point { ptX :: Int, ptY :: Int } deriving (Eq,Show)

collides :: Shape -> Shape -> Bool
collides (Rectangle x y w h) (Rectangle x1 y1 w1 h1) = (inRange x w x1 w1) && (inRange y h y1 h1)
	where
		inRange p1 l1 p2 l2 = (p1 <= p2) && (p2 <= p1 + l1) || (p2 <= p1) && (p1 <= p2 +l2)
collides (Point x y) (Point x1 y1) = (x == x1) && (y == y1)
collides (Rectangle x y w h) (Point x1 y1) = (inRange x1 x w) && (inRange y1 y h)
	where
		inRange p1 p2 l2 = (p2 <= p1) && (p1 <= p2 + l2)
collides p@(Point _ _) r@(Rectangle _ _ _ _) = collides r p
collides l1@(Line (x',y') (x1',y1')) l2@(Line (x2',y2') (x3',y3')) = if l1 == l2 then True else (if (crossDir == 0) then False else (0 <= t) && (t <= 1) && (0 <= u) && (u <= 1))
	where
		crossDir = dx*dy1 - dx1*dy
		t = ((x2-x)*dy1 - (y2-y)*dx1) /  (dx*dy1 - dx1*dy)
		u = ((x2-x)*dy - (y2-y)*dx) /  (dx*dy1 - dx1*dy)
		dx = x1 - x
		dy = y1 - y
		dx1 = x3 - x2
		dy1 = y3 - y2
		x = fromIntegral x'
		x1 = fromIntegral x1'
		x2 = fromIntegral x2'
		x3 = fromIntegral x3'
		y = fromIntegral y'
		y1 = fromIntegral y1'
		y2 = fromIntegral y2'
		y3 = fromIntegral y3'
collides (Point x' y') (Line (lx',ly') (lx1',ly1')) = abs((y - ly1) - ((ly1 - ly)/(lx1 - lx)) * (x - lx1)) < 0.1
	where
		x = fromIntegral x'
		y = fromIntegral y'
		lx = fromIntegral lx'
		ly = fromIntegral ly'
		lx1 = fromIntegral lx1'
		ly1 = fromIntegral ly1'
collides l@(Line _ _) p@(Point _ _) = collides p l
collides (Rectangle x y w h) l@(Line _ _) = (collides (Line (x,y) (x+w,y)) l) ||
	(collides (Line (x,y) (x,y+h)) l) ||
	(collides (Line (x+w,y) (x+w,y+h)) l) ||
	(collides (Line (x,y+h) (x+w,y+h)) l)
collides l@(Line _ _) r@(Rectangle _ _ _ _) = collides r l
