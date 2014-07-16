data Decoder a = 
    FailH !B.ByteString !ByteOffset String
  | PartialH (B.ByteString -> Decoder a)
  | DoneH !B.ByteString !ByteOffset a

data SequenceDecoder a = 
    FailS !B.ByteString !ByteOffset String
  | ManyS [a] (B.ByteString -> SequenceDecoder a)
  | DoneS !B.ByteString !ByteOffset [a]

newtype eventLogDecoder = Decoder (SequenceDecoder Event)


main = do
  log <- readLogInc file
  putStrLn $ ppEventLog log

