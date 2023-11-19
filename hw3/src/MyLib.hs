module MyLib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.IO

-- ref: https://github.com/gltronred/itis-fp-fall-2023/tree/trunk/p1
-- 16/11: https://my.mts-link.ru/82082257/518533452/record-new/1169032133/record-file/709573617
-- дз в сб

-- трансформеры на полиморфизме?
-- MonadTrans
-- StateT -> параметр m это монада
-- комбинируем несколько эффектов
-- lift -> вычисление во внутренней монаде поднимаем до трансформера

-- порядок (стека монад) фиксируется заранее при проектировании что не оч удобно
-- явно лифтим -> можно повторять эффекты несколько раз


-- Отделяем побочные эффекты "состояние" от "IO"
-- Можем комбинировать в рамках одной программы

getNextNumber :: IO Int
getNextNumber = do
  putStrLn "Next number, please"
  read <$> getLine

-- Вводить с клавиатуры числа
-- Суммировать в состояние (сумма, количество)
-- При вводе -1 вернём среднее
comp :: StateT (Int,Int) IO Double
comp = do
  x <- lift getNextNumber
  (s,n) <- get
  case x of
    -1 -> pure $ fromIntegral s / fromIntegral n
    _ -> do
      put (s + x, n + 1)
      comp

someFunc :: IO ()
someFunc = do
  avg <- evalStateT comp (0,0)
  putStrLn $ "Average: " ++ show avg