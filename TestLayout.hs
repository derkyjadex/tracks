module TestLayout where

import Tracks.Network
import Tracks.Layout
import Text.Printf (printf)

runLayout :: Network -> FilePath -> Int -> IO ()
runLayout network outputPath iterations =
        let g = buildGraph network
            gs = iterate applyForces g
            ngs = zip [0..] gs
         in mapM_ write $ take iterations ngs
      where write (n, g) =
                let name = printf "%05d" (n :: Int)
                    path = outputPath ++ "/" ++ name ++ ".svg"
                    content = toSvg g
                 in do
                     writeFile path content
                     if n `mod` 100 == 0
                         then print n
                         else return ()

testLayout :: FilePath -> FilePath -> Int -> IO ()
testLayout networkPath outputPath iterations = do
        network <- readTracksFile networkPath
        runLayout network outputPath iterations

toSvg :: Graph -> String
toSvg g =
        "<?xml version=\"1.0\" standalone = \"no\"?>\n" ++
        "<?xml-stylesheet type=\"text/css\" href=\"style.css\"?>\n" ++
        "<svg width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        "  <style></style>\n" ++
        svgInfo g ++
        concatMap (svgEdge g) (edges g) ++
        concatMap (svgVertex g) (vertices g) ++
        "</svg>"

svgVertex :: Graph -> Vertex -> String
svgVertex g v =
        let (x, y) = vertexPosition g v
         in "  <line class=\"vertex\" x1=\"" ++ show x ++ "\" y1=\"" ++ show y ++ "\" x2=\"" ++ show x ++ "\" y2=\"" ++ show y ++ "\" stroke=\"hsl(" ++ show (vertexColour g v) ++ ", 100%, 60%)\"/>\n" ++
            "  <text class=\"label\" x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\">" ++ xmlSafe (show v) ++ "</text>\n"
      where xmlSafe ('&':cs) = "&amp;" ++ xmlSafe cs
            xmlSafe (c:cs) = c : xmlSafe cs
            xmlSafe [] = []

svgEdge :: Graph -> (Vertex, Vertex) -> String
svgEdge g (a, b) =
        let (ax, ay) = vertexPosition g a
            (bx, by) = vertexPosition g b
         in "  <line class=\"edge\" x1=\"" ++ show ax ++ "\" y1=\"" ++ show ay ++ "\" x2=\"" ++ show bx ++ "\" y2=\"" ++ show by ++ "\"/>\n"

svgInfo :: Graph -> String
svgInfo g =
        "  <text id=\"info\" x=\"5\" y=\"" ++ show (height - 5) ++ "\">n = " ++ show (n g) ++ ", temperature = " ++ show (temperature g) ++ "</text>\n"
