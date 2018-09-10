{-# LANGUAGE QuasiQuotes #-}

module Day1 () where

import           Data.Char         (digitToInt)
import           Text.RawString.QQ
-- note https://kseo.github.io/posts/2014-02-06-multi-line-strings-in-haskell.html

input= filter (/= '\n') [r|4281224989975872839961169513979579335691369498483794171253625322698694611857431137339923313
79856446362482129646556286611543756564275715359874924898113424472782974789464348626278532936228881
78627358627888657582823936679442922331747672233742439923998615367527592411332256187381436445133918
691881345168526319289162718676981812871559571544456544458151467752187493594291354712175185163137331
612249147156469773129895198951191727268433546343621828326196215867126662529918876458981451879357637
562916389634966531299128577659514214626179224447572178294136478796892453784169853828845935515978398
5638187254653851864874544584878999193242641611859756728634623853475638478923744471563845635468173824
1966843619342694594591242691968115129274426627615638243236217587858663914247786835991794478455959319
28589255935953295111937431266815352781399967295389339626178664148415561175386725992469782888757942558
362117938629369129439717427474416851628121191639355646394276451847131182652486561415942815818785884559
193483878139351841633366398788657844396925423217662517356486193821341454889283266691224778723833397914
2243967225595939591253171758995946855248524194957933894818313547872874523671456618292875187716319393146
8313772249353131818131521634299414168348411196947695294637831488342167795239758861356295874132898773456
5492378977396431481215983656814486518865642645612413945129485464979535991675776338786758997128124651311
1531828161889249351863618137972519976439926862947246992819694731427211164329682164349776841381844819638
4514148679399647679395422622588543242265439443988284216329545854975513724761433899187996666592546654511
18997149437165711133264794329259392279967999512794857228367544577376681918459145667322859284537818187922
36447816127492445993945894435692799839217467253986218213131249786833333936332257795191937942688668182629
489191693154184177398186462481316834678733713614889439352976144726162214648922159719979143735815478633912
633185334529484779322818611438194522292278787653763328944421516569181178517915745625295158611636365253948455727653672922299582352766484|]


compute :: String -> Int
compute xs = sum $  (digitToInt . fst) <$>  filter (uncurry (==)) (zip xs xs')
  where
    xs' = drop 1 $ cycle xs

tc1 =  compute "1122" -- 3
tc2 = compute "1111" -- produces 4 because each digit (all 1) matches the next.
tc3 = compute "1234" -- produces 0 because no digit matches the next.
tc4 = compute "91212129" -- produces 9 because the only digit that matches the next one is the last digit, 9
-- input 1034
