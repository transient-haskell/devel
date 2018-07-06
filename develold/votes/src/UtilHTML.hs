{-# OPTIONS -XOverlappingInstances  #-}
{-# LINE 2 "UtilHTML.hs" #-}
module UtilHTML where
import  Data.Text.Lazy as T
{-# LINE 3 "UtilHTML.hs" #-}
import Data.List
{-# LINE 4 "UtilHTML.hs" #-}
import HSP.XMLGenerator
{-# LINE 5 "UtilHTML.hs" #-}
import Data
{-# LINE 6 "UtilHTML.hs" #-}
import Vote
{-# LINE 7 "UtilHTML.hs" #-}
import Lang
{-# LINE 8 "UtilHTML.hs" #-}
import LangEs
{-# LINE 9 "UtilHTML.hs" #-}
import HTTPParser ((->>))
{-# LINE 11 "UtilHTML.hs" #-}
showChanges l
  = (genElement (Nothing, pack "p") []
       [asChild
          ([(genElement (Nothing,pack "p") []
               [asChild ((genElement (Nothing,pack "b") [] [asChild ("Deleted:")])),
                asChild ((genEElement (Nothing,pack "br") [])), asChild (x),
                asChild ((genEElement (Nothing,pack "br") [])),
                asChild ((genElement (Nothing,pack "b") [] [asChild ("Added")])),
                asChild ((genEElement (Nothing,pack "br") [])), asChild (y),
                asChild ((genEElement (Nothing,pack "br") []))])
            | Change _ xs ys <- l, x <- xs, y <- ys])])
{-# LINE 28 "UtilHTML.hs" #-}
change2HTML xs diff
  = (genElement (Nothing,pack "p") []
       [asChild (Prelude.concatMap showHTML1 $ diff2Changes xs diff 0),
        asChild (" ")])
  where {-# LINE 30 "UtilHTML.hs" #-}
        showHTML1 (Normal x)
          = Prelude.map (\ t -> (genElement (Nothing,pack "p") [] [asChild (t)])) x
        {-# LINE 31 "UtilHTML.hs" #-}
        showHTML1 (Deleted x)
          = Prelude.map
              (\ t ->
                 (genElement (Nothing,pack "p") []
                    [asChild
                       ((genElement (Nothing,pack "strike") []
                           [asChild (" "), asChild (t)]))]))
              x
        {-# LINE 32 "UtilHTML.hs" #-}
        showHTML1 (Added x)
          = Prelude.map
              (\ t ->
                 (genElement (Nothing,pack "font")
                    [asAttr ("color" := "FF0000"), asAttr ("size" := "3")]
                    [asChild (t), asChild ((genEElement (Nothing,pack "br") []))]))
              x

{-# LINE 35 "UtilHTML.hs" #-}
--blankRow :: HSP XML
{-# LINE 36 "UtilHTML.hs" #-}
blankRow
  = (genElement (Nothing,pack "tr") []
       [asChild
          ((genElement (Nothing,pack "td") []
              [asChild ((genEElement (Nothing,pack "br") []))]))])

{-# LINE 37 "UtilHTML.hs" #-}
--vsep :: HSP XML
{-# LINE 38 "UtilHTML.hs" #-}
vsep
  = (genElement (Nothing,pack "p") []
       [asChild ((genEElement (Nothing,pack "br") [])),
        asChild ((genElement (Nothing,pack "center") [] [asChild ("-.-")])),
        asChild ((genEElement (Nothing,pack "br") []))])
{-# LINE 42 "UtilHTML.hs" #-}
tableStyle
  = ["border" := "3", "cellspacing" := "0", "width" := "100%",     "id" := "AutoNumber1", "style" := "border-collapse: collapse",
     "bordercolor" := "#111111", "cellpadding" := "2"]
{-# LINE 48 "UtilHTML.hs" #-}
pageTableStyle = ["width" := "100%", "cellspacing" := "9"]
{-# LINE 50 "UtilHTML.hs" #-}
listByComma
  = Prelude.foldr (\ a b -> if Prelude.null b then a else a ++ ", " ++ b) ""
{-# LINE 54 "UtilHTML.hs" #-}
showTextContent str = Vote.replace str "\n" "<br/>"
{-# LINE 57 "UtilHTML.hs" #-}
showContent lang s
  = case (content s) of
        Str string -> (genElement (Nothing,pack "span") []
                         [asChild (" "),
                          asChild
                            ([(genElement (Nothing,pack "p") [] [asChild (l)]) |
                              l <- Prelude.lines string]),
                          asChild ("pack ")])
        ConfText string -> (genElement (Nothing, "span") []
                              [asChild (" "),
                               asChild
                                 ([(genElement (Nothing,pack "p") [] [asChild (l)]) |
                                   l <- Prelude.lines string]),
                               asChild (" ")])
        Changes changes -> showChanges changes
{-# LINE 65 "UtilHTML.hs" #-}
voteCond s rangev
  | rangev >= 0 = True
  | otherwise = False
{-# LINE 68 "UtilHTML.hs" #-}
voteForm lang email s rangev project
  = do hasVoted <- hasVotedUser email (votes s) project
       return $
         if voteCond s rangev then
           (genElement (Nothing,pack "p") []
              [asChild
                 (voteForm1 lang "vote" "" (sname s) hasVoted
                    (stringOptions lang s))])
           else (genEElement (Nothing,pack "p") [])

{-# LINE 79 "UtilHTML.hs" #-}
--voteForm1 ::
--          String ->
--            String ->
--              String -> String -> Maybe PriorIVote -> [String] -> HSP XML
{-# LINE 80 "UtilHTML.hs" #-}
voteForm1 lang op aname subject hasVoted options
  = (genElement (Nothing,pack "p") []
       [asChild ((genElement (Nothing,pack "b") [] [asChild (tvotethis)])),
        asChild ((genEElement (Nothing,pack "a") [asAttr ("name" := "vote")])),
        asChild ((genEElement (Nothing,pack "br") [])),
        asChild
          ((genElement (Nothing,pack "center") []
              [asChild ((genElement (Nothing,pack "h4") [] [asChild (toptionst)])),
               asChild
                 ((genElement (Nothing,pack "form")
                     [asAttr ("action" := cgiURL), asAttr ("method" := "post")]
                     [asChild
                        ((genEElement (Nothing,pack "input")
                            [asAttr ("type" :=pack "hidden"), asAttr ("name" := "op"),
                             asAttr ("value" := op)])),
                      asChild
                        ((genEElement (Nothing,pack "input")
                            [asAttr ("type" := "hidden"), asAttr ("name" := "subject"),
                             asAttr ("value" := subject)])),
                      asChild
                        (if op == "voteam" then
                           (genEElement (Nothing,pack "input")
                              [asAttr ("type" := "hidden"), asAttr ("name" := "aname"),
                               asAttr ("value" := aname)])
                           else (genEElement (Nothing,pack "span") [])),
                      asChild
                        ((genElement (Nothing,pack "table") []
                            [asChild
                               ([(genElement (Nothing,pack "tr") []
                                    [asChild
                                       ((genElement (Nothing,pack "td") []
                                           [asChild
                                              ((genEElement (Nothing,pack "input")
                                                  [asAttr ("type" := "radio"),
                                                   asAttr ("name" := "option"),
                                                   asAttr ("value" := value)]))])),
                                     asChild
                                       ((genElement (Nothing,pack "td") [] [asChild (formatOp value)])),
                                     asChild
                                       ((genElement (Nothing,pack "td") []
                                           [asChild
                                              ((genElement (Nothing,pack "a")
                                                  [asAttr
                                                     ("href" :=
                                                        (cgiURL ++ "?op=mess&opt=" ++ value ++
                                                           "&subject="
                                                           ++ subject))]
                                                  [asChild (tsupportmess)]))]))])
                                 | value <- options]),
                             asChild
                               (if op == "vote" then
                                  (genElement (Nothing,pack "tr") []
                                     [asChild
                                        ((genElement (Nothing,pack "td") [] [asChild (tyoucanalso)])),
                                      asChild ((genElement (Nothing,pack "td") [] [])),
                                      asChild
                                        ((genElement (Nothing,pack "td") []
                                            [asChild
                                               ((genElement (Nothing,pack "a")
                                                   [asAttr
                                                      ("href" :=
                                                         (cgiURL ++ "?op=sub&subject=" ++ subject ++
                                                            "&type=amend"))]
                                                   [asChild
                                                      ((genElement (Nothing,pack "b") []
                                                          [asChild (tsuggestmodif)]))]))]))])
                                  else (genElement (Nothing,pack "tr") [] []))])),
                      asChild ((genEElement (Nothing,pack "br") [])),
                      asChild ((genEElement (Nothing,pack "br") [])),
                      asChild
                        ((genEElement (Nothing,pack "input")
                            [asAttr ("type" := "submit"), asAttr ("name" := "Vote"),
                             asAttr ("value" := tvote)]))]))]))])
  where {-# LINE 124 "UtilHTML.hs" #-}
        formatOp op
          = case (Data.List.isPrefixOf "http" $ Prelude.dropWhile (== ' ') s2) of
                True -> (genElement (Nothing,pack "a") [asAttr ("href" := s2)]
                           [asChild (" "),
                            asChild ((genElement (Nothing,pack "b") [] [asChild (s1)]))])
                False -> (genElement (Nothing,pack "span") []
                            [asChild ((genElement (Nothing,pack "b") [] [asChild (s1)])),
                             asChild (" "), asChild ("- "), asChild (s2)])
          where {-# LINE 128 "UtilHTML.hs" #-}
                (s1, (c : s2))
                  = case (findIndex (== ',') op) of
                        Just i -> Prelude.splitAt i op
                        Nothing -> (op, ",")
        {-# LINE 131 "UtilHTML.hs" #-}
        (toptionst, tvotethis, tvote, tsuggestmodif, tyoucanalso,
         tsupportmess)
          = case lang of
                "es" -> (LangEs.optionst, LangEs.votethis, LangEs.vote,
                         LangEs.suggestmodif, LangEs.youcanalso, LangEs.supportmess)
                _ -> (Lang.optionst, Lang.votethis, Lang.vote, Lang.suggestmodif,
                      Lang.youcanalso, Lang.supportmess)
{-# LINE 137 "UtilHTML.hs" #-}
amendOptions lang
  = case lang of
        "es" -> [LangEs.yes, LangEs.no, LangEs.complaint]
        _ -> [Lang.yes, Lang.no, Lang.complaint]
{-# LINE 142 "UtilHTML.hs" #-}
hiddenCodify env
  = (genElement (Nothing,pack "p") []
       [asChild
          ([(genEElement (Nothing,pack "input")
               [asAttr ("type" := "hidden"), asAttr ("name" := name),
                asAttr ("value" := value)])
            | (name, value) <- env1])])
  where {-# LINE 143 "UtilHTML.hs" #-}
        (env1, _) = Vote.myreads1 env (\ (x, y) -> x /= "HTTP_COOKIE")
{-# LINE 159 "UtilHTML.hs" #-}
moreProjects email
  = case email of
        "" -> do pub <- publicProjectNames
                 return $ Prelude.take 3 pub
        email -> do prs <- projectListNames email
                    let {-# LINE 164 "UtilHTML.hs" #-}
                        l = Prelude.length prs
                    if l < 3 then
                      do pub <- publicProjectNames
                         return $ prs ++ Prelude.take (3 - l) pub
                      else return $ Prelude.take 3 prs
{-# LINE 169 "UtilHTML.hs" #-}
moreSubjects email
  = do prs <- projectListNames email
       return $ Prelude.take 3 prs

{-# LINE 175 "UtilHTML.hs" #-}
--createTabs :: [String] -> String -> String -> [String] -> HSP XML
{-# LINE 176 "UtilHTML.hs" #-}
createTabs ants op msg options
  = (genElement (Nothing,pack "div") [asAttr ("id" := "divTabs")]
       [asChild
          ((genElement (Nothing,pack "ul") []
              [asChild
                 ([(genElement (Nothing,pack "li") [asAttr ("class" := "")]
                      [asChild (" "),
                       asChild
                         ((genElement (Nothing,pack "a") [asAttr ("href" := (link i))]
                             [asChild
                                ((genElement (Nothing,pack "span") [asAttr ("class" := "tabName")]
                                    [asChild (" "), asChild (opt), asChild (" ")])),
                              asChild
                                ((genElement (Nothing,pack "span") [asAttr ("class" := "arrow")]
                                    [asChild (" "), asChild ("> ")]))])),
                       asChild (" ")])
                   | (opt, i) <- Prelude.zip ants [1 ..]]),
               asChild
                 ((genElement (Nothing,pack "li") [asAttr ("class" := "selected")]
                     [asChild
                        ((genElement (Nothing,pack "span") [asAttr ("class" := "tabName")]
                            [asChild (op)]))])),
               asChild
                 ((genEElement (Nothing,pack "li") [asAttr ("class" := "blank")])),
               asChild
                 ((genElement (Nothing,pack "li") [asAttr ("class" := "blank")]
                     [asChild (" "), asChild (msg), asChild (" ")])),
               asChild
                 ([(genElement (Nothing,pack "li") [asAttr ("class" := "")]
                      [asChild
                         ((genElement (Nothing,pack "a") [asAttr ("href" := (link1))]
                             [asChild
                                ((genElement (Nothing,pack "span") [asAttr ("class" := "tabName")]
                                    [asChild (i)]))]))])
                   | i <- options])]))])
  where {-# LINE 189 "UtilHTML.hs" #-}
        h = ["none", "project", "subject"]
        {-# LINE 190 "UtilHTML.hs" #-}
        zhant = Prelude.zip h ants
        {-# LINE 191 "UtilHTML.hs" #-}
        link1 = linkTo zhant
        {-# LINE 192 "UtilHTML.hs" #-}
        link i = linkTo $ Prelude.zip h (Prelude.take i ants ++ repeat "")
        {-# LINE 193 "UtilHTML.hs" #-}
        linkTo cs = cgiURL ++ "?" ++ Prelude.concatMap linkTo1 cs
          where {-# LINE 194 "UtilHTML.hs" #-}
                linkTo1 (n, v) = n ++ "=" ++ v ++ "&"
