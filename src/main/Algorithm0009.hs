import System.Environment

main = do args <- getArgs
          let count = head args -- ���̗v�f���̈����v��Ȃ��C������
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
sortCow list =
    let (ahead, nextList) = popElement list
    in  ahead ++  sortCow nextList

{- |
 - �w�肳�ꂽ���X�g�̐擪�Ɩ����̗v�f���r���A
 - ��菬�������̗v�f�����X�g����؂�o���܂��B
 -
 - * �؂�o���̃��[���͈ȉ��̒ʂ�B
 - 1. �擪 �� ���� : �擪��؂�o��
 - 2. �擪 �� ���� : ������؂�o��
 - 3. �擪 �� ���� : �擪���������؂�o��
 - 
 - * �ԋp�l�̃^�v���̒�`�͈ȉ��̒ʂ�B
 - ��Ԗڂ̗v�f: �؂�o���ꂽ�v�f���X�g(�����v�f����\��������)
 - ��Ԗڂ̗v�f: �؂�o���ꂽ�c��̗v�f�����������X�g
 -}
popElement
    :: [Char]
    -> ([Char], [Char])
popElement [] = ([], [])
popElement (lst:[]) = (lst:[], [])
popElement list 
    | headElm < lastElm = (headElm:[], tail list)
    | headElm > lastElm = (lastElm:[], init list)
    | otherwise = (headElm:lastElm:[], init $ tail list)
    where headElm = head list
          lastElm = last list
