import Graphics.UI.WX
import Graphics.UI.WXCore
import Test.HUnit

main :: IO ()
main = start $ do

  f <- frame []
  t <- treeCtrl f []

  root <- treeCtrlAddRoot t "root" (-1) (-1) objectNull
  _    <- treeCtrlAppendItem t root "item1" (-1) (-1) objectNull
  _    <- treeCtrlAppendItem t root "item2" (-1) (-1) objectNull
  _    <- treeCtrlAppendItem t root "item3" (-1) (-1) objectNull
  treeCtrlExpand t root

  cs <- treeCtrlGetChildren t root
  -- set f [ layout := fill $ widget t ]
  runTestTT $ TestCase $ assertEqual "tree children" 3 (length cs)
