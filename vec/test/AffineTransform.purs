module Test.Data.Vec3.AffineTransform where

import Prelude
import Data.Vec3
import Data.Vec3.AffineTransform
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Math (pi, sqrt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

shouldVecApproxEqual :: ∀ m. MonadThrow Error m => Vec3 Number -> Vec3 Number -> m Unit
shouldVecApproxEqual l r = l `shouldSatisfy` vec3ApproxEqual r

shouldAffineTransformApproxEqual :: ∀ m. MonadThrow Error m => AffineTransform Number -> AffineTransform Number -> m Unit
shouldAffineTransformApproxEqual l r = l `shouldSatisfy` affineTransformApproxEqual r

spec :: Spec Unit
spec = do
  describe "Affine transformations" do
    it "should rotate correctly" do
      let vec = vec2 1.0 0.0
      let rot180 = rotate pi `transform` vec
      rot180 `shouldVecApproxEqual` vec2 (-1.0) 0.0
      let rot90 = rotate (pi / 2.0) `transform` vec
      rot90 `shouldVecApproxEqual` vec2 0.0 1.0
      let rot60 = rotate (pi / 3.0) `transform` vec
      rot60 `shouldVecApproxEqual` vec2 0.5 (sqrt 0.75)
      let rot45 = rotate (pi / 4.0) `transform` vec
      rot45 `shouldVecApproxEqual` vec2 (sqrt 0.5) (sqrt 0.5)

    it "should translate points, not vectors" do
      let vec = translate (vec2 1.0 2.0) `transform` (vec2 3.0 5.0)
      vec `shouldVecApproxEqual` vec2 3.0 5.0
      let pt = translate (vec2 1.0 2.0) `transform` (point2 3.0 5.0)
      pt `shouldVecApproxEqual` point2 4.0 7.0

    it "should compose transformations by multipying" do
      let pt = point2 3.0 5.0
      let xlate = translate (vec2 1.0 2.0)
      let rot = rotate (pi / 2.0)
      ((rot * xlate) `transform` pt) `shouldVecApproxEqual` (rot `transform` (xlate `transform` pt))
      ((xlate * rot) `transform` pt) `shouldVecApproxEqual` (xlate `transform` (rot `transform` pt))

    it "should invert correctly" do
      let at = translate (vec2 1.0 2.0) * scale 2.0 * rotate (pi / 4.0)
      let atInv = rotate (-pi / 4.0) * scale 0.5 * translate (-vec2 1.0 2.0)
      inverse at `shouldAffineTransformApproxEqual` atInv
