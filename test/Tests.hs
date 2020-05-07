import Test.DocTest

main :: IO ()
main = doctest ["-XOverloadedLists", "-XOverloadedStrings", "-isrc", "src/Zipper.hs"]
