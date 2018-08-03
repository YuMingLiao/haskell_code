textify :: (Show a, Data a, Typeable a) => a -> Text
textify a = case cast a of
                 Just (x :: String) -> pack x
                 Nothing -> case cast a of
                                 Just (x :: Text) -> x
                                 Nothing -> pack (show a)


