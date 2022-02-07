{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module DearImGui.Structs where

-- base
import Data.Word
  ( Word32, Word16 )
import Foreign
  ( Storable(..), castPtr, plusPtr )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Foreign.C.Types

import DearImGui.Enums
--------------------------------------------------------------------------------
data ImVec2 = ImVec2 { x, y :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec2 where
  sizeOf ~ImVec2{x, y} = sizeOf x + sizeOf y

  alignment _ = 0

  poke ptr ImVec2{ x, y } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    return ImVec2{ x, y  }


data ImVec3 = ImVec3 { x, y, z :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec3 where
  sizeOf ~ImVec3{x, y, z} = sizeOf x + sizeOf y + sizeOf z

  alignment _ = 0

  poke ptr ImVec3{ x, y, z } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    return ImVec3{ x, y, z }


data ImVec4 = ImVec4 { x, y, z, w :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec4 where
  sizeOf ~ImVec4{x, y, z, w} = sizeOf x + sizeOf y + sizeOf z + sizeOf w

  alignment _ = 0

  poke ptr ImVec4{ x, y, z, w } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z
    poke (castPtr ptr `plusPtr` (sizeOf x * 3)) w

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    w <- peek (castPtr ptr `plusPtr` (sizeOf x * 3))
    return ImVec4{ x, y, z, w }

--------------------------------------------------------------------------------

-- | DearImGui context handle.
data ImGuiContext

-- | Individual font handle.
data ImFont

-- | Font configuration handle.
data ImFontConfig

-- | Glyph ranges builder handle.
data ImFontGlyphRangesBuilder

-- | Opaque DrawList handle.
data ImDrawList

-- | 'DearImGui.Raw.ListClipper.ListClipper' pointer tag.
data ImGuiListClipper

-- | 32-bit unsigned integer (often used to store packed colors).
type ImU32 = Word32

-- | Single wide character (used mostly in glyph management)
type ImWchar = Word16
-- FIXME: consider IMGUI_USE_WCHAR32

data ImGuiStyle =
  ImGuiStyle { styleAlpha :: {-# unpack #-} !Float
             , styleDisableAlpha :: {-# unpack #-} !Float
             , styleWindowPadding :: {-# unpack #-} !ImVec2
             , styleWindowRounding :: {-# unpack #-} !Float
             , styleWindowBorderSize :: {-# unpack #-} !Float
             , styleWindowMinSize :: {-# unpack #-} !ImVec2
             , styleWindowTitleAlign :: {-# unpack #-} !ImVec2
             , styleWindowMenuButtonPosition :: {-# unpack #-} !ImGuiDir
             , styleChildRounding :: {-# unpack #-} !Float
             , styleChildBorderSize :: {-# unpack #-} !Float
             , stylePopupRounding :: {-# unpack #-} !Float
             , stylePopupBorderSize :: {-# unpack #-} !Float
             , styleFramePadding :: {-# unpack #-} !ImVec2
             , styleFrameRounding :: {-# unpack #-} !Float
             , styleFrameBorderSize :: {-# unpack #-} !Float
             , styleItemSpacing :: {-# unpack #-} !ImVec2
             , styleItemInnerSpacing :: {-# unpack #-} !ImVec2
             , styleCellPadding :: {-# unpack #-} !ImVec2
             , styleTouchExtraPadding :: {-# unpack #-} !ImVec2
             , styleIndentSpacing :: {-# unpack #-} !Float
             , styleColumnsMinSpacing :: {-# unpack #-} !Float
             , styleScrollbarSize :: {-# unpack #-} !Float
             , styleScrollbarRounding :: {-# unpack #-} !Float
             , styleGrabMinSize :: {-# unpack #-} !Float
             , styleGrabRounding :: {-# unpack #-} !Float
             , styleLogSliderDeadzone :: {-# unpack #-} !Float
             , styleTabRounding :: {-# unpack #-} !Float
             , styleTabBorderSize :: {-# unpack #-} !Float
             , styleTabMinWidthForCloseButton :: {-# unpack #-} !Float
             , styleColorButtonPosition :: {-# unpack #-} !ImGuiDir
             , styleButtonTextAlign :: {-# unpack #-} !ImVec2
             , styleSelectableTextAlign :: {-# unpack #-} !ImVec2
             , styleDisplayWindowPadding :: {-# unpack #-} !ImVec2
             , styleDisplaySafeAreaPadding :: {-# unpack #-} !ImVec2
             , styleMouseCursorScale :: {-# unpack #-} !Float
             , styleAntiAliasedLines :: !Bool
             , styleAntiAliasedLinesUseTex :: !Bool
             , styleAntiAliasedFill :: !Bool
             , styleCurveTessellationTol :: {-# unpack #-} !Float
             , styleCircleTessellationMaxError :: {-# unpack #-} !Float
            --  , stylePointSize :: {-# unpack #-} !Float
            --  , stylePointSizeRound :: {-# unpack #-} !Float
             , styleColors :: ![ImVec4] }
    deriving (Show)

instance Storable ImGuiStyle where
  -- ASSERT: sizeOf ImGuiStyle{..} == 0x418
  sizeOf ~ImGuiStyle{..} = sum
      [ sizeOf styleAlpha
      , sizeOf styleDisableAlpha
      , sizeOf styleWindowPadding
      , sizeOf styleWindowRounding
      , sizeOf styleWindowBorderSize
      , sizeOf styleWindowMinSize
      , sizeOf styleWindowTitleAlign
      , sizeOf styleWindowMenuButtonPosition
      , sizeOf styleChildRounding
      , sizeOf styleChildBorderSize
      , sizeOf stylePopupRounding
      , sizeOf stylePopupBorderSize
      , sizeOf styleFramePadding
      , sizeOf styleFrameRounding
      , sizeOf styleFrameBorderSize
      , sizeOf styleItemSpacing
      , sizeOf styleItemInnerSpacing
      , sizeOf styleCellPadding
      , sizeOf styleTouchExtraPadding
      , sizeOf styleIndentSpacing
      , sizeOf styleColumnsMinSpacing
      , sizeOf styleScrollbarSize
      , sizeOf styleScrollbarRounding
      , sizeOf styleGrabMinSize
      , sizeOf styleGrabRounding
      , sizeOf styleLogSliderDeadzone
      , sizeOf styleTabRounding
      , sizeOf styleTabBorderSize
      , sizeOf styleTabMinWidthForCloseButton
      , sizeOf styleColorButtonPosition
      , sizeOf styleButtonTextAlign
      , sizeOf styleSelectableTextAlign
      , sizeOf styleDisplayWindowPadding
      , sizeOf styleDisplaySafeAreaPadding
      , sizeOf styleMouseCursorScale
      -- , sizeOf styleAntiAliasedLines
      -- , sizeOf styleAntiAliasedLinesUseTex
      -- , sizeOf styleAntiAliasedFill
      , 4
      , sizeOf styleCurveTessellationTol
      , sizeOf styleCircleTessellationMaxError
      -- , sizeOf stylePointSize
      -- , sizeOf stylePointSizeRound
      , sizeOf (head styleColors) * fromIntegral (count @ImGuiCol) ]
  alignment _ = 0
  poke ptr ImGuiStyle {..} = flip evalStateT 0 $ do
    poke' styleAlpha
    poke' styleDisableAlpha
    poke' styleWindowPadding
    poke' styleWindowRounding
    poke' styleWindowBorderSize
    poke' styleWindowMinSize
    poke' styleWindowTitleAlign
    poke' styleWindowMenuButtonPosition
    poke' styleChildRounding
    poke' styleChildBorderSize
    poke' stylePopupRounding
    poke' stylePopupBorderSize
    poke' styleFramePadding
    poke' styleFrameRounding
    poke' styleFrameBorderSize
    poke' styleItemSpacing
    poke' styleItemInnerSpacing
    poke' styleCellPadding
    poke' styleTouchExtraPadding
    poke' styleIndentSpacing
    poke' styleColumnsMinSpacing
    poke' styleScrollbarSize
    poke' styleScrollbarRounding
    poke' styleGrabMinSize
    poke' styleGrabRounding
    poke' styleLogSliderDeadzone
    poke' styleTabRounding
    poke' styleTabBorderSize
    poke' styleTabMinWidthForCloseButton
    poke' styleColorButtonPosition
    poke' styleButtonTextAlign
    poke' styleSelectableTextAlign
    poke' styleDisplayWindowPadding
    poke' styleDisplaySafeAreaPadding
    poke' styleMouseCursorScale
    poke' $ toCBool styleAntiAliasedLines
    poke' $ toCBool styleAntiAliasedLinesUseTex
    poke' $ toCBool styleAntiAliasedFill
    modify (+ 1) -- Struct packing space
    poke' styleCurveTessellationTol
    poke' styleCircleTessellationMaxError
    -- poke' stylePointSize
    -- poke' stylePointSizeRound
    mapM_ poke' styleColors
    where
      poke' :: Storable a => a -> StateT Int IO ()
      poke' value = do
        offset <- get
        modify (+ sizeOf value)
        liftIO $ poke (castPtr ptr `plusPtr` offset) value
      toCBool True = CBool 1
      toCBool False = CBool 0
  peek ptr =
    flip evalStateT 0 $
      ImGuiStyle <$> peek' styleAlpha
                 <*> peek' styleDisableAlpha
                 <*> peek' styleWindowPadding
                 <*> peek' styleWindowRounding
                 <*> peek' styleWindowBorderSize
                 <*> peek' styleWindowMinSize
                 <*> peek' styleWindowTitleAlign
                 <*> peek' styleWindowMenuButtonPosition
                 <*> peek' styleChildRounding
                 <*> peek' styleChildBorderSize
                 <*> peek' stylePopupRounding
                 <*> peek' stylePopupBorderSize
                 <*> peek' styleFramePadding
                 <*> peek' styleFrameRounding
                 <*> peek' styleFrameBorderSize
                 <*> peek' styleItemSpacing
                 <*> peek' styleItemInnerSpacing
                 <*> peek' styleCellPadding
                 <*> peek' styleTouchExtraPadding
                 <*> peek' styleIndentSpacing
                 <*> peek' styleColumnsMinSpacing
                 <*> peek' styleScrollbarSize
                 <*> peek' styleScrollbarRounding
                 <*> peek' styleGrabMinSize
                 <*> peek' styleGrabRounding
                 <*> peek' styleLogSliderDeadzone
                 <*> peek' styleTabRounding
                 <*> peek' styleTabBorderSize
                 <*> peek' styleTabMinWidthForCloseButton
                 <*> peek' styleColorButtonPosition
                 <*> peek' styleButtonTextAlign
                 <*> peek' styleSelectableTextAlign
                 <*> peek' styleDisplayWindowPadding
                 <*> peek' styleDisplaySafeAreaPadding
                 <*> peek' styleMouseCursorScale
                 <*> peek' styleAntiAliasedLines
                 <*> peek' styleAntiAliasedLinesUseTex
                 <*> peek' styleAntiAliasedFill
                 <*> peek' styleCurveTessellationTol
                 <*> peek' styleCircleTessellationMaxError
                --  <*> peek' stylePointSize
                --  <*> peek' stylePointSizeRound
                 <*> forM [0..count @ImGuiCol - 1] (const $ peek' (head . styleColors))
    where
        peek' :: Storable a => (ImGuiStyle -> a) -> StateT Int IO a
        peek' accessor = do
            offset <- get
            modify (+ sizeOf (accessor undefined))
            liftIO $ peek (castPtr ptr `plusPtr` offset)
