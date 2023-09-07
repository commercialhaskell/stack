import Control.Monad (when)

import Src
import B

main = when (funMainLib 41 /= funInternal 43) $ error "test failed"
