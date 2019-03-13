import System.IO
import Test.Hspec
import Crawler

main :: IO ()
main = do
    putStrLn "Tests not implemented yet"
    -- Link Extractor
    --describe "Link Extractor" $ do
        --it "Extracts links correctly from dummy file"

loadDummyHtml :: IO String
loadDummyHtml = readFile ".\\test\\static\\test.html"