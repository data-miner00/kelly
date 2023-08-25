module ProveMap2D
    ( map2d
    ) where


map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

-- Derivation
map2d' = (\f1 xs -> map f1 xs) . (\f2 ys -> map f2 ys)

map2d'' = (\x -> (\f1 xs -> map f1 xs) ((\f2 ys -> map f2 ys) x))

map2d''' x = (\f1 xs -> map f1 xs) ((\f2 ys -> map f2 ys) x)

map2d'''' x = (\f1 xs -> map f1 xs) (\ys -> map x ys)

map2d''''' x = (\xs -> map (\ys -> map x ys) xs)

map2d'''''' f xs = map (\ys -> map f ys) xs
