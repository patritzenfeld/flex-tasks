{-# language TypeOperators #-}

{- | Functions for adding Css and JavaScript to rendered forms.
-}

module FlexTask.Content (
  addCss,
  addJs,
  addCssAndJs,
) where


import Data.Tuple.Extra                (second)
import Text.Cassius                    (Css)
import Text.Julius                     (Javascript)
import Yesod (ToWidget(..))
import Yesod.Core.Types                (RY)

import FlexTask.YesodConfig (
  FlexForm(..),
  Rendered',
  Widget,
  )



applyToWidget :: Functor m => (Widget -> Widget) -> Rendered' m -> Rendered' m
applyToWidget f form = fmap (second f) <$> form


addContent
  :: (ToWidget FlexForm (render -> a), Functor m)
  => (render -> a)
  -> Rendered' m
  -> Rendered' m
addContent content = applyToWidget (<* toWidget content)


{- |
Add CSS to a form.
Use with `Yesod` Cassius or Lucius Shakespeare quasi quoters or hosted files.
-}
addCss
  :: (render ~ RY FlexForm, Functor m)
  => (render -> Css) -- ^ CSS template
  -> Rendered' m     -- ^ Form to add to
  -> Rendered' m
addCss = addContent


{- |
Add JavaScript to a form.
Use with `Yesod` Julius Shakespeare quasi quoters or hosted files.
-}
addJs
  :: (render ~ RY FlexForm, Functor m)
  => (render -> Javascript) -- ^ Javascript template
  -> Rendered' m            -- ^ Form to add to
  -> Rendered' m
addJs = addContent


{- |
Like `addCss` and `addJs`, but for including CSS and JavaScript in one step.
-}
addCssAndJs
  :: (render ~ RY FlexForm, Functor m)
  => (render -> Css)        -- ^ CSS template
  -> (render -> Javascript) -- ^ Javascript template
  -> Rendered' m            -- ^ Form to add to
  -> Rendered' m
addCssAndJs css js = applyToWidget ((<* toWidget css) . (<* toWidget js))
