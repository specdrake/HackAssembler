bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s
