-----------------------------------------------------------------------------------------
{-|
Module      :  Print
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Printer abstraction layer. See @samples\/wx\/Print.hs@ for a demo.

The application should create a 'pageSetupDialog' to hold the printer
settings of the user.

> f <- frame [text := "Print demo"]                               
> 
> -- Create a pageSetup dialog with an initial margin of 25 mm.
> pageSetup <- pageSetupDialog f 25

The dialog can be shown using 'pageSetupShowModal'. Furthermore, the 
function 'printDialog' and 'printPreview' can be used to show a print dialog
and preview window.

> mprint   <- menuItem file 
>                [ text := "&Print..."
>                , help := "Print a test"
>                , on command := printDialog pageSetup "Test"  pageFun printFun
>                ]
> mpreview <- menuItem file 
>                [ text := "&Print preview"
>                , help := "Print preview"
>                , on command := printPreview pageSetup "Test" pageFun printFun 

Those functions take a 'PageFunction' and 'PrintFunction' respectively that get called
to determine the number of needed pages and to draw on the printer DC respectively.
The framework takes automatic care of printer margins, preview scaling etc.

-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Print( -- * Printing
                                 pageSetupDialog
                               , pageSetupShowModal
                               , printDialog
                               , printPreview
                                 -- * Callbacks
                               , PageFunction
                               , PrintFunction
                                 -- * Page and printer info
                               , PageInfo(..)
                               , PrintInfo(..)
                                 -- * Internal
                               , pageSetupDataGetPageInfo, pageSetupDataSetPageInfo
                               , printOutGetPrintInfo
                               , pageSetupDialogGetFrame
                               ) where

import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassInfo
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.Frame

-- | Return a page range given page info, print info, and the printable size.
-- The printable size is the number of pixels available for printing 
-- without the page margins.
type PageFunction    = PageInfo -> PrintInfo -> Size -> (Int,Int)

-- | Print a page given page info, print info, the printable size, the
-- printer device context and the current page.
-- The printable size is the number of pixels available for printing 
-- without the page margins
type PrintFunction   = PageInfo -> PrintInfo -> Size -> DC () -> Int -> IO ()


{--------------------------------------------------------------------------
   Handle print events
--------------------------------------------------------------------------}  
-- | The standard print event handler
onPrint :: Bool {- preview? -} 
            -> PageInfo -> Printout (CWXCPrintout a)
            -> PageFunction
            -> PrintFunction 
            -> EventPrint -> IO ()
onPrint isPreview pageInfo printOut pageRangeFunction printFunction ev 
  = case ev of
      PrintPrepare ->
        printOutInitPageRange printOut pageInfo pageRangeFunction >>
        return ()

      PrintPage _cancel dc n ->
        do{ printInfo <- printOutGetPrintInfo printOut
          ; let io info size = printFunction pageInfo info size dc n 
          ; if isPreview 
             then do let previewInfo = toScreenInfo printInfo
                     (scaleX,scaleY) <- getPreviewZoom pageInfo previewInfo dc                     
                     dcScale dc scaleX scaleY (respectMargin pageInfo previewInfo dc (io previewInfo))
             else respectMargin pageInfo printInfo dc (io printInfo)
          }        
      _ -> return ()


-- | Set a clipping region and device origin according to the margin
respectMargin :: PageInfo -> PrintInfo -> DC a -> (Size -> IO b) -> IO b
respectMargin pageInfo printInfo dc io
  = do let ((left,top),printSize) = printableArea pageInfo printInfo

       -- the device origin is in unscaled coordinates
       scaleX <- dcGetUserScaleX dc
       scaleY <- dcGetUserScaleY dc
       dcSetDeviceOrigin dc (pt (round (scaleX*left)) (round (scaleY*top)))

       -- the clipping respects the scaling
       dcSetClippingRegion dc (rect (pt 0 0) printSize)
       io printSize

-- | Calculate the printable area
printableArea :: PageInfo -> PrintInfo -> ((Double,Double),Size)
printableArea pageInfo printInfo
  = let (printW,printH) = pixelToMM (printerPPI printInfo) (printPageSize printInfo)
        (ppmmW,ppmmH)   = ppiToPPMM (printerPPI printInfo)

        -- calculate minimal printer margin
        minX  = (toDouble (sizeW (pageSize pageInfo)) - printW)/2  
        minY  = (toDouble (sizeH (pageSize pageInfo)) - printH)/2

        -- top-left margin
        top   = ppmmH * (max minY (toDouble $ rectTop  $ pageArea pageInfo))
        left  = ppmmW * (max minX (toDouble $ rectLeft $ pageArea pageInfo))

        -- bottom-right margin
        (Point mright mbottom) 
             = pointSub (pointFromSize (pageSize pageInfo)) (rectBottomRight (pageArea pageInfo))          
        bottom= ppmmH * (max minY (toDouble mbottom))
        right = ppmmW * (max minX (toDouble mright))

        dw = round (right + left)
        dh = round (bottom + top)
        (dw', dh') = if sizeW (printPageSize printInfo) < sizeH (printPageSize printInfo)
                     then (dw, dh)
                     else (dh, dw)

        -- the actual printable page size
        printSize = sz (sizeW (printPageSize printInfo) - dw') 
                      (sizeH (printPageSize printInfo) - dh')
    in ((left,top),printSize) 

-- | Get the zoom factor from the preview 
getPreviewZoom :: PageInfo -> PrintInfo -> DC a -> IO (Double,Double)
getPreviewZoom _pageInfo printInfo dc
  = do size <- dcGetSize dc
       let (printW,printH)   = pixelToMM (printerPPI printInfo) (printPageSize printInfo)
           (screenW,screenH) = pixelToMM (screenPPI printInfo) size
           scaleX       = screenW / printW
           scaleY       = screenH / printH
       return (scaleX,scaleY)


-- | Transform printer info to screen printer info (for the preview).
toScreenInfo :: PrintInfo -> PrintInfo
toScreenInfo printInfo
  = let scaleX  = (toDouble (sizeW (screenPPI printInfo))) / (toDouble (sizeW (printerPPI printInfo)))
        scaleY  = (toDouble (sizeH (screenPPI printInfo))) / (toDouble (sizeH (printerPPI printInfo)))
        pxX     = round (scaleX * (toDouble (sizeW (printPageSize printInfo))))
        pxY     = round (scaleY * (toDouble (sizeH (printPageSize printInfo))))
    in printInfo{ printerPPI    = screenPPI printInfo
                , printPageSize = sz pxX pxY
                }

-- | Pixels to millimeters given a PPI
pixelToMM :: Size -> Size -> (Double,Double)
pixelToMM ppi size
  = let convert f  = toDouble (f size) / (toDouble (f ppi) / 25.4)
    in (convert sizeW, convert sizeH)

-- | pixels per inch to pixels per millimeter
ppiToPPMM :: Size -> (Double,Double)
ppiToPPMM ppi
  = let convert f  = toDouble (f ppi) / 25.4
    in (convert sizeW, convert sizeH)

-- | Convert an 'Int' to a 'Double'.
toDouble :: Int -> Double
toDouble i = fromIntegral i

-- | Scale the 'DC'.
dcScale :: DC a -> Double -> Double -> IO b -> IO b
dcScale dc scaleX scaleY io
  = do oldX <- dcGetUserScaleX dc
       oldY <- dcGetUserScaleY dc
       dcSetUserScale dc (oldX*scaleX) (oldY*scaleY)
       x <- io
       dcSetUserScale dc oldX oldY
       return x

{--------------------------------------------------------------------------
  preview and printIt
--------------------------------------------------------------------------}
-- | Show a print dialog.
printDialog :: PageSetupDialog a 
          -> String
          -> PageFunction
          -> PrintFunction 
          -> IO ()
printDialog pageSetupDialog' title pageRangeFunction printFunction =
  do{ pageSetupData    <- pageSetupDialogGetPageSetupData pageSetupDialog'
    ; printData        <- pageSetupDialogDataGetPrintData pageSetupData
    ; printDialogData  <- printDialogDataCreateFromData printData
    ; printDialogDataSetAllPages printDialogData True 
    ; printer          <- printerCreate printDialogData
    ; printout         <- wxcPrintoutCreate title
    ; pageInfo         <- pageSetupDataGetPageInfo pageSetupData
    ; _                <- printOutInitPageRange printout pageInfo pageRangeFunction
    ; printOutOnPrint printout (onPrint False pageInfo printout pageRangeFunction printFunction)
    ; frame            <- pageSetupDialogGetFrame pageSetupDialog'
    ; _                <- printerPrint printer frame printout True {- show printer setup? -}
    ; objectDelete printDialogData
    ; objectDelete printout
    ; objectDelete printer
    }

-- | Show a preview window
printPreview :: PageSetupDialog a 
           -> String
           -> PageFunction
           -> PrintFunction
           -> IO ()
printPreview pageSetupDialog' title pageRangeFunction printFunction =
  do{ pageSetupData <- pageSetupDialogGetPageSetupData pageSetupDialog'
    ; pageInfo      <- pageSetupDataGetPageInfo pageSetupData
    ; printout1     <- wxcPrintoutCreate "Print to preview"
    ; printout2     <- wxcPrintoutCreate "Print to printer"
    ; startPage     <- printOutInitPageRange printout1 pageInfo pageRangeFunction
    ; _             <- printOutInitPageRange printout2 pageInfo pageRangeFunction
    ; printOutOnPrint printout1 (onPrint True  pageInfo printout1 pageRangeFunction printFunction)
    ; printOutOnPrint printout2 (onPrint False pageInfo printout2 pageRangeFunction printFunction)
    ; printData        <- pageSetupDialogDataGetPrintData pageSetupData
    ; printDialogData  <- printDialogDataCreateFromData printData
    ; printDialogDataSetAllPages printDialogData True 
    ; preview      <- printPreviewCreateFromDialogData printout1 printout2 printDialogData
    ; _            <- printPreviewSetCurrentPage preview startPage
    ; frame        <- pageSetupDialogGetFrame pageSetupDialog'
    ; previewFrame <- previewFrameCreate preview frame title rectNull frameDefaultStyle title
    ; previewFrameInitialize previewFrame
    ; _            <- windowShow previewFrame 
    ; windowRaise previewFrame
    }


{--------------------------------------------------------------------------
  Class helpers
--------------------------------------------------------------------------}

-- | Set the correct page range for a printout.
printOutInitPageRange :: WXCPrintout a -> PageInfo -> PageFunction -> IO Int
printOutInitPageRange printOut pageInfo pageRangeFunction
  = do{ printInfo <- printOutGetPrintInfo printOut
      ; let (_,size)    = printableArea pageInfo printInfo
            (start,end) = pageRangeFunction pageInfo printInfo size
      ; wxcPrintoutSetPageLimits printOut start end start end
      ; return start
      }


-- | Get the parent frame of a 'PageSetupDialog'.
pageSetupDialogGetFrame :: PageSetupDialog a -> IO (Frame ())
pageSetupDialogGetFrame pageSetupDialog'
  = do p <- windowGetParent pageSetupDialog' 
       case (safeCast p classFrame) of
        Just frame  -> return frame
        Nothing     -> do w <- wxcAppGetTopWindow
                          case (safeCast w classFrame) of
                            Just frame -> return frame
                            Nothing    -> error "pageSetupDialogGetFrame: no parent frame found!"


{--------------------------------------------------------------------------
    PageSetupDialog  
--------------------------------------------------------------------------}  
-- | Create a (hidden) page setup dialog that remembers printer settings.
-- It is a parameter to the functions 'printDialog' and 'printPreview'.
-- The creation function takes a parent frame and the initial page margins
-- (in millimeters) as an argument.
pageSetupDialog :: Frame a -> Int -> IO (PageSetupDialog ())
pageSetupDialog f margin
  = do pageSetupData  <- pageSetupDialogDataCreate
       if (margin > 0)
        then do pageInfo <- pageSetupDataGetPageInfo pageSetupData
                let p0      = pt margin margin
                    p1      = pointSub (pointFromSize (pageSize pageInfo)) p0
                    newInfo = pageInfo{ pageArea = rectBetween p0 p1 }
                pageSetupDataSetPageInfo pageSetupData newInfo
        else return ()                                                                                           
       pageSetupDialog' <- pageSetupDialogCreate f pageSetupData
       prev <- windowGetOnClose f
       windowOnClose f (do{ objectDelete pageSetupDialog'; prev })
       objectDelete pageSetupData
       return pageSetupDialog'

-- | Show the page setup dialog
pageSetupShowModal :: PageSetupDialog a -> IO ()
pageSetupShowModal p
  = dialogShowModal p >> return ()

{--------------------------------------------------------------------------
  PageInfo and PrintInfo
--------------------------------------------------------------------------}

-- | Information from the page setup dialog.
--   All measurements are in millimeters.
data PageInfo = PageInfo{ pageSize :: Size  -- ^ The page size (in millimeters)
                        , pageArea :: Rect  -- ^ The available page area (=margins) (in millimeters)
                        } 
                        deriving Show

-- | Get page info
pageSetupDataGetPageInfo :: PageSetupDialogData a  -> IO PageInfo
pageSetupDataGetPageInfo pageSetupData 
  = do{ topLeft     <- pageSetupDialogDataGetMarginTopLeft pageSetupData
      ; bottomRight <- pageSetupDialogDataGetMarginBottomRight pageSetupData
      ; paperSize   <- pageSetupDialogDataGetPaperSize pageSetupData
      ; return (PageInfo
          { pageSize   = paperSize
          , pageArea   = rectBetween topLeft (pointSub (pointFromSize paperSize) bottomRight)
          })
      }

-- | Set page info
pageSetupDataSetPageInfo :: PageSetupDialogData a -> PageInfo -> IO ()
pageSetupDataSetPageInfo pageSetupData pageInfo
  = do{ let topLeft     = rectTopLeft (pageArea pageInfo)
            bottomRight = pointSub (pointFromSize (pageSize pageInfo)) (rectBottomRight (pageArea pageInfo))
      ; pageSetupDialogDataSetMarginTopLeft pageSetupData topLeft
      ; pageSetupDialogDataSetMarginBottomRight pageSetupData bottomRight
      ; pageSetupDialogDataSetPaperSize pageSetupData (pageSize pageInfo)
      }


-- | Printer information.
data PrintInfo = PrintInfo  { screenPPI         :: Size -- ^ screen pixels per inch
                            , printerPPI        :: Size -- ^ printer pixels per inch
                            , printPageSize     :: Size -- ^ printable area (in pixels) = PageInfo pageSize minus printer margins
                            } deriving Show

-- | Extract print info    
printOutGetPrintInfo :: Printout a -> IO PrintInfo
printOutGetPrintInfo printOut 
  = do{ thePrinterPPI     <- printoutGetPPIPrinter printOut
      ; theScreenPPI      <- printoutGetPPIScreen printOut
      ; thePageSizePixels <- printoutGetPageSizePixels printOut
      ; return (PrintInfo 
          { printerPPI  = sizeFromPoint thePrinterPPI
          , screenPPI   = sizeFromPoint theScreenPPI
          , printPageSize = thePageSizePixels
          })
      } 

