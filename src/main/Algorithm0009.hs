import System.Environment

main = do args <- getArgs
          let count = head args
          let list = head $ tail args
          putStrLn $ show $ sortCow list
{- |
 - �w�肳�ꂽ�����̃��X�g���\�[�g���܂��B
 - 
 - ���X�g�̐擪�v�f�Ɩ����v�f���r���Ȃ���A
 - �������l�����v�f��擪�ւƕ��בւ��Ă����܂��B
 -}
sortCow
    :: [Char]
    -> [Char]
sortCow [] = []
sortCow list 
    | needsLast == True = (last list) : sortCow (init list)
    | needsLast == False = (head list) : sortCow (tail list)
    where needsLast = isHeadLarger list

{- |
 - �w�肳�ꂽ���X�g�̐擪�Ɩ����̗v�f���r���A
 - �擪�v�f���傫�����ǂ����𔻒肵�܂��B
 -
 - * ����̃��[���͈ȉ��̒ʂ�B
 - 1. �擪 �� ���� : False
 - 2. �擪 �� ���� : True
 - 3. �擪 �� ���� : �擪�Ɩ��������������X�g���ċA�I�ɔ��肷��
 -}
isHeadLarger
    :: [Char]
    -> Bool
isHeadLarger [] = False
isHeadLarger (end:[]) = False
isHeadLarger list 
    | headElm < lastElm = False
    | headElm > lastElm = True
    | otherwise = isHeadLarger (init $ tail list)
    where headElm = head list
          lastElm = last list


