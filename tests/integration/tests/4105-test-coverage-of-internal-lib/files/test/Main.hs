import           Control.Monad ( when )

import Lib ( funcLib )
import Internal ( funcInternal )

main = when (funcLib 41 /= funcInternal 43) $ error "test failed"
