module Markup
    ( Document
    , Structure(..)
    )
    where
import Numeric.Natural

type Document
    = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrdereedList [String]
    | CodeBlock [String]
