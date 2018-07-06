
{-# LINE 2 "Pages.hs" #-}
module Pages where
{-# LINE 3 "Pages.hs" #-}
import Control.Exception
{-# LINE 4 "Pages.hs" #-}
import Data.Maybe (fromJust, fromMaybe, catMaybes)
{-# LINE 5 "Pages.hs" #-}
import Data.Array
{-# LINE 6 "Pages.hs" #-}
import System.Time
{-# LINE 7 "Pages.hs" #-}
import System.Locale
{-# LINE 8 "Pages.hs" #-}
import System.Directory
{-# LINE 9 "Pages.hs" #-}
import System.IO.Unsafe
{-# LINE 10 "Pages.hs" #-}
import Data.List (find, findIndex, isPrefixOf, dropWhile, (\\),
                  lines, sortBy)
{-# LINE 11 "Pages.hs" #-}
import Data.Array.IArray (Array, array, (//), (!), bounds, elems)
{-# LINE 12 "Pages.hs" #-}
import Text.Printf
{-# LINE 13 "Pages.hs" #-}
import Debug.Trace
{-# LINE 16 "Pages.hs" #-}
import HSP.XML
{-# LINE 19 "Pages.hs" #-}
import Data
{-# LINE 20 "Pages.hs" #-}
import Vote
{-# LINE 21 "Pages.hs" #-}
--import Help
--{-# LINE 22 "Pages.hs" #-}
import Lang
{-# LINE 23 "Pages.hs" #-}
import LangEs
{-# LINE 24 "Pages.hs" #-}
import UtilHTML
{-# LINE 26 "Pages.hs" #-}
import HTTPParser
{-# LINE 27 "Pages.hs" #-}
import Aprobal
{-# LINE 28 "Pages.hs" #-}
import Conf

{-# LINE 32 "Pages.hs" #-}
pageBodyPr :: HSP XML -> String -> HSP XML -> HSP XML -> HSP XML
{-# LINE 33 "Pages.hs" #-}
pageBodyPr tabs header rigthHtml body
  = (genElement (Nothing, "table") [asAttr ("id" := "tablebody")]
       [asChild
          ((genElement (Nothing, "tr") []
              [asChild ((genElement (Nothing, "td") [] [asChild (tabs)]))])),
        asChild
          ((genElement (Nothing, "tr") []
              [asChild
                 ((genElement (Nothing, "td") []
                     [asChild
                        ((genElement (Nothing, "p") [asAttr ("id" := "intro")]
                            [asChild (" "), asChild (header), asChild (" ")]))]))])),
        asChild
          ((genElement (Nothing, "tr") []
              [asChild ((genElement (Nothing, "td") [] [asChild (body)])),
               asChild
                 ((genElement (Nothing, "td") [asAttr ("valign" := "top")]
                     [asChild (rigthHtml)]))]))])
{-# LINE 57 "Pages.hs" #-}
leftSide lang
  = (genElement (Nothing, "p") []
       [asChild
          ((genElement (Nothing, "a")
              [asAttr ("href" := (cgiURL ++ "?project="))]
              [asChild
                 ((genElement (Nothing, "h4") [] [asChild (tyourgroups)]))])),
        asChild
          ((genElement (Nothing, "a") [asAttr ("href" := cgiURL)]
              [asChild (" "),
               asChild
                 ((genElement (Nothing, "h4") [] [asChild (tproposals)]))])),
        asChild ((genElement (Nothing, "h4") [] [asChild (tdelegation)])),
        asChild (validateWidget lang),
        asChild ((genEElement (Nothing, "br") [])),
        asChild
          ((genElement (Nothing, "a")
              [asAttr ("href" := (cgiURL ++ "?op=login"))]
              [asChild (" "),
               asChild
                 ((genElement (Nothing, "h4") []
                     [asChild (tlogout), asChild (" ")]))]))])
  where {-# LINE 67 "Pages.hs" #-}
        (tlogout, tyourgroups, tproposals, tdelegation)
          = case lang of
                "es" -> (LangEs.logout, LangEs.yourgroups, LangEs.proposals,
                         LangEs.delegation)
                _ -> (Lang.logout, Lang.yourgroups, Lang.proposals,
                      Lang.delegation)
{-# LINE 71 "Pages.hs" #-}
rigthSide lang
  = (genElement (Nothing, "p") []
       [asChild
          ((genElement (Nothing, "h4") [] [asChild (thelp), asChild (" ")])),
        asChild
          ((genElement (Nothing, "h4") []
              [asChild (twhoweare), asChild (" ")])),
        asChild
          ((genElement (Nothing, "a")
              [asAttr ("href" := (cgiURL ++ "?op=phy"))]
              [asChild (" "),
               asChild
                 ((genElement (Nothing, "h4") []
                     [asChild (" "), asChild (Lang.philosophy)])),
               asChild (" ")])),
        asChild
          ((genElement (Nothing, "h4") []
              [asChild (" "), asChild (tfeedback), asChild (" ")])),
        asChild
          ((genElement (Nothing, "h4") []
              [asChild (" "), asChild (tchangelog), asChild (" ")]))])
  where {-# LINE 79 "Pages.hs" #-}
        (thelp, twhoweare, tfeedback, tchangelog)
          = case lang of
                "es" -> (LangEs.help, LangEs.whoweare, LangEs.feedback,
                         LangEs.changelog)
                _ -> (Lang.help, Lang.whoweare, Lang.feedback, Lang.changelog)
{-# LINE 88 "Pages.hs" #-}
pageBody tabs lang body
  = pageBodyPr tabs tintro (rigthSide lang) body
  where {-# LINE 89 "Pages.hs" #-}
        tintro
          = case lang of
                "es" -> LangEs.intro
                _ -> Lang.intro
{-# LINE 93 "Pages.hs" #-}
philosophyPage
  = do let {-# LINE 94 "Pages.hs" #-}
           tabs
             = createTabs [freeChooserName] "Philosophy" "Groups:"
                 ["Groups", "last group", "last proposal"]
       Content{mime =
                 page "philosophy" $ pageBody tabs "en" Help.philosophy,
               cookies = []}
{-# LINE 98 "Pages.hs" #-}
errorPage e
  = Content{mime =
              page "Error" $
                (genElement (Nothing, "p") []
                   [asChild
                      ((genElement (Nothing, "b") []
                          [asChild (" "), asChild ("Sorry, an error has ocurred:   ")])),
                    asChild
                      (case e of
                           ErrorCall errorTxt -> errorTxt
                           _ -> "The error has been logged."),
                    asChild ((genEElement (Nothing, "br") [])),
                    asChild
                      ((genElement (Nothing, "a") [asAttr ("href" := cgiURL)]
                          [asChild (" "),
                           asChild ("click here to return to the home page ")]))]),
            cookies = []}
  where {-# LINE 113 "Pages.hs" #-}
        tabs = createTabs [freeChooserName] "Error" "" []
{-# LINE 116 "Pages.hs" #-}
modifyProjectPage env cookies msg
  = do if null email then
         return $ registrationPage (("backto", "modifyProject") : env) ""
         else
         case types of
             "create" -> modifyProjectPage1 env cookies blankProject msg
             "modify" -> do Just (Rp pr) <- getVResource
                                              (Rp uProject{pname = project})
                            if email `elem` users pr then modifyProjectPage1 env cookies pr msg
                              else userPage env [] tyounotperm
  where {-# LINE 127 "Pages.hs" #-}
        types = env ->> "type"
        {-# LINE 128 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 129 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 130 "Pages.hs" #-}
        tyounotperm
          = case env ->> acceptLang of
                "es" -> LangEs.younotperm
                _ -> Lang.younotperm

{-# LINE 134 "Pages.hs" #-}
page :: String -> HSP XML -> HSP XML
{-# LINE 135 "Pages.hs" #-}
page title bodyhtml
  = (genElement (Nothing, "html") []
       [asChild
          ((genElement (Nothing, "head") []
              [asChild ((genElement (Nothing, "title") [] [asChild (title)])),
               asChild
                 ((genEElement (Nothing, "link")
                     [asAttr ("rel" := "stylesheet"), asAttr ("type" := "text/css"),
                      asAttr ("href" := "style.css")]))])),
        asChild ((genElement (Nothing, "body") [] [asChild (bodyhtml)]))])
{-# LINE 147 "Pages.hs" #-}
modifyProjectPage1 env cookies pr msg
  = do return
         Content{mime =
                   page "Create/modify group" $ pageBody tabs lang $ body pr,
                 cookies = cookies}
  where {-# LINE 156 "Pages.hs" #-}
        tabs = createTabs tabs types "" []
          where {-# LINE 158 "Pages.hs" #-}
                tabs
                  = case types of
                        "modify" -> [freeChooserName, project]
                        _ -> [freeChooserName]
        {-# LINE 162 "Pages.hs" #-}
        lang = env ->> acceptLang
        {-# LINE 163 "Pages.hs" #-}
        body pr
          = (genElement (Nothing, "p") []
               [asChild
                  ((genElement (Nothing, "h1") []
                      [asChild ("Editing group: " ++ (pname pr)), asChild (" ")])),
                asChild
                  ((genElement (Nothing, "font")
                      [asAttr ("color" := "FF0000"), asAttr ("size" := "4")]
                      [asChild (" "),
                       asChild ((genElement (Nothing, "b") [] [asChild (msg)])),
                       asChild (" ")])),
                asChild
                  ((genElement (Nothing, "form")
                      [asAttr ("action" := (cgiURL ++ "?op=cre")),
                       asAttr ("method" := "post")]
                      [asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
                              asAttr ("value" := "cre")])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "oldname"),
                              asAttr ("value" := (if types == "modify" then project else ""))])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "type"),
                              asAttr ("value" := types)])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild (" "), asChild (tthispagecollect), asChild (" ")])),
                       asChild (vsep),
                       asChild
                         (if types /= "modify" then
                            (genElement (Nothing, "p") []
                               [asChild
                                  ((genElement (Nothing, "b") [] [asChild (tpleaseentername)])),
                                asChild (texplainname), asChild ((genEElement (Nothing, "br") [])),
                                asChild
                                  ((genElement (Nothing, "p") [asAttr ("align" := "center")]
                                      [asChild
                                         ((genEElement (Nothing, "input")
                                             [asAttr ("type" := "text"), asAttr ("size" := "75"),
                                              asAttr ("name" := "name"),
                                              asAttr ("value" := (pname pr))]))])),
                                asChild (vsep)])
                            else
                            (genEElement (Nothing, "input")
                               [asAttr ("type" := "hidden"), asAttr ("name" := "name"),
                                asAttr ("value" := (pname pr))])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild
                                ((genElement (Nothing, "b") [] [asChild (tpleasedescrip)])),
                              asChild (texplaindescrip)])),
                       asChild
                         ((genElement (Nothing, "center") []
                             [asChild
                                ((genElement (Nothing, "textarea")
                                    [asAttr ("name" := "pdescrip"), asAttr ("rows" := "20"),
                                     asAttr ("cols" := "65"), asAttr ("warp" := "true")]
                                    [asChild ((pdescrip pr)), asChild (" ")]))])),
                       asChild (vsep),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild
                                ((genElement (Nothing, "b") []
                                    [asChild (tpleasetopics), asChild (" ")])),
                              asChild (texplaintopics), asChild (" ")])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "center")]
                             [asChild
                                ((genElement (Nothing, "textarea")
                                    [asAttr ("name" := "topics"), asAttr ("rows" := "3"),
                                     asAttr ("cols" := "65"), asAttr ("wrap" := "True")]
                                    [asChild (listByComma (ptopics pr)), asChild (" ")]))])),
                       asChild (vsep),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild
                                ((genElement (Nothing, "b") [] [asChild (tpleaseemails)])),
                              asChild (texplainemails)])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "center")]
                             [asChild
                                ((genElement (Nothing, "textarea")
                                    [asAttr ("name" := "users"), asAttr ("rows" := "10"),
                                     asAttr ("cols" := "65"), asAttr ("wrap" := "True")]
                                    [asChild (concatMap ("\n" ++) $ users pr), asChild (" ")]))])),
                       asChild (vsep),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "subjects"),
                              asAttr ("value" := (concatMap ("\n" ++) $ psubjects pr))])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild
                                ((genEElement (Nothing, "input")
                                    [asAttr ("type" := "checkbox"),
                                     asAttr ("checked" := checkPublic),
                                     asAttr ("name" := "ispublic"), asAttr ("value" := "OFF")])),
                              asChild ((genElement (Nothing, "b") [] [asChild (tpleasepublic)])),
                              asChild (" "), asChild (texplainpublic)])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild
                                ((genEElement (Nothing, "input")
                                    [asAttr ("type" := "checkbox"),
                                     asAttr ("checked" := checkVisible),
                                     asAttr ("name" := "isvisible"), asAttr ("value" := "OFF")])),
                              asChild
                                ((genElement (Nothing, "b") [] [asChild (tpleasevisible)])),
                              asChild (" "), asChild (texplainvisible)])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "submit"), asAttr ("name" := "OK"),
                              asAttr ("value" := "OK")])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "reset"), asAttr ("name" := "cancel"),
                              asAttr ("value" := "cancel")]))]))])
        {-# LINE 205 "Pages.hs" #-}
        checkPublic = show (public pr)
        {-# LINE 207 "Pages.hs" #-}
        checkVisible = show (visible pr)
        {-# LINE 209 "Pages.hs" #-}
        types = env ->> "type"
        {-# LINE 210 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 212 "Pages.hs" #-}
        (tpleasevisible, tpleasepublic, tpleaseemails, tpleasetopics,
         tpleasedescrip, tpleaseentername, tthispagecollect, texplainpublic,
         texplainvisible, texplaintopics, texplaindescrip, texplainemails,
         texplainname)
          = case env ->> acceptLang of
                "es" -> (LangEs.pleasevisible, LangEs.pleasepublic,
                         LangEs.pleaseemails, LangEs.pleasetopics, LangEs.pleasedescrip,
                         LangEs.pleaseentername, LangEs.thispagecollect,
                         LangEs.explainpublic, LangEs.explainvisible, LangEs.explaintopics,
                         LangEs.explaindescrip, LangEs.explainemails, LangEs.explainname)
                _ -> (Lang.pleasevisible, Lang.pleasepublic, Lang.pleaseemails,
                      Lang.pleasetopics, Lang.pleasedescrip, Lang.pleaseentername,
                      Lang.thispagecollect, Lang.explainpublic, Lang.explainvisible,
                      Lang.explaintopics, Lang.explaindescrip, Lang.explainemails,
                      Lang.explainname)
{-# LINE 216 "Pages.hs" #-}
modifySubjectPage env cookies msg
  = if null email then
      return $ validationPage (("backto", "modifySubject") : env) "" else
      do Rp pr <- justGetVResource (Rp uProject{pname = project})
         Just (Rs s) <- case types of
                            "amend" -> do Just (Rs s) <- getVResource $!
                                                           Rs
                                                             uSubject{sname = ssname,
                                                                      prosname = project}
                                          return $! Just $ Rs s{authors = [email]}
                            "modify" -> getVResource $!
                                          Rs uSubject{sname = ssname, prosname = project}
                            "create" -> return $! Just $
                                          Rs
                                            blankSubject{prosname = project, authors = [email],
                                                         lastVote = tnow}
         if email `elem` users pr then modifySubjectPage1 env cookies s msg
           else userPage env [] tyounotperm
  where {-# LINE 231 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 232 "Pages.hs" #-}
        ssname = env ->> "subject"
        {-# LINE 233 "Pages.hs" #-}
        types = env ->> "type"
        {-# LINE 234 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 235 "Pages.hs" #-}
        tyounotperm
          = case env ->> acceptLang of
                "es" -> LangEs.younotperm
                _ -> Lang.younotperm
{-# LINE 240 "Pages.hs" #-}
modifySubjectPage1 env cookies s msg
  = do conf <- getProjectConf $ prosname s
       let {-# LINE 242 "Pages.hs" #-}
           lcategories = categories conf `debug` (show conf)
       Rp pr <- justGetVResource $ Rp uProject{pname = prosname s} `debug`
                  ("project= " ++ prosname s)
       return
         Content{mime =
                   page "create/modify a Subject" $ pageBody tabs lang $
                     body pr s year month day hour lcategories,
                 cookies = cookies}
  where {-# LINE 251 "Pages.hs" #-}
        tabs = createTabs tabs (types ++ " proposal") "" []
          where {-# LINE 254 "Pages.hs" #-}
                tabs
                  = case types of
                        "create" -> [freeChooserName, prosname s]
                        _ -> [freeChooserName, prosname s, sname s]

        {-# LINE 257 "Pages.hs" #-}
        body ::
             Project ->
               Subject -> Int -> Month -> Int -> Int -> [String] -> HSP XML
        {-# LINE 258 "Pages.hs" #-}
        body pr s year month day hour lcategories
          = (genElement (Nothing, "p") []
               [asChild
                  ((genElement (Nothing, "form")
                      [asAttr ("action" := cgiURL), asAttr ("method" := "post")]
                      [asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
                              asAttr ("value" := "suc")])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "type"),
                              asAttr ("value" := types)])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "backTo"),
                              asAttr ("value" := (env ->> "backTo"))])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "project"),
                              asAttr ("value" := (prosname s))])),
                       asChild
                         ((genElement (Nothing, "font")
                             [asAttr ("color" := "FF0000"), asAttr ("size" := "4")]
                             [asChild (" "), asChild (msg), asChild (" ")])),
                       asChild
                         ((genElement (Nothing, "h1") []
                             [asChild
                                (case types of
                                     "modify" -> teditingsub
                                     "create" -> tpropcreation
                                     "amend" -> "Amending: "
                                   ++ (sname s))])),
                       asChild (vsep),
                       asChild
                         (case types of
                              "create" -> (genElement (Nothing, "p") []
                                             [asChild
                                                ((genElement (Nothing, "p")
                                                    [asAttr ("align" := "justify")]
                                                    [asChild
                                                       ((genElement (Nothing, "b") []
                                                           [asChild (tpleasename)])),
                                                     asChild (" "), asChild (texplainnamesub),
                                                     asChild (" ")])),
                                              asChild
                                                ((genElement (Nothing, "p")
                                                    [asAttr ("align" := "center")]
                                                    [asChild
                                                       ((genEElement (Nothing, "input")
                                                           [asAttr ("type" := "textfield"),
                                                            asAttr ("size" := "80"),
                                                            asAttr ("name" := "name"),
                                                            asAttr ("value" := "")]))])),
                                              asChild (vsep),
                                              asChild
                                                ((genElement (Nothing, "p")
                                                    [asAttr ("align" := "justify")]
                                                    [asChild (" "),
                                                     asChild
                                                       ((genElement (Nothing, "b") []
                                                           [asChild (tpleasecategory),
                                                            asChild (" ")])),
                                                     asChild (texplaincategory), asChild (" ")])),
                                              asChild
                                                ([(genElement (Nothing, "p") []
                                                     [asChild
                                                        ((genEElement (Nothing, "input")
                                                            [asAttr ("type" := "radio"),
                                                             asAttr
                                                               ("checked" :=
                                                                  (show $
                                                                     if x == category s then True
                                                                       else False)),
                                                             asAttr ("name" := "category"),
                                                             asAttr ("value" := (x))])),
                                                      asChild (" "),
                                                      asChild
                                                        ((genElement (Nothing, "b") []
                                                            [asChild (x)]))])
                                                  | x <- lcategories])])
                              "amend" -> (genElement (Nothing, "p") []
                                            [asChild
                                               ((genElement (Nothing, "p")
                                                   [asAttr ("align" := "justify")]
                                                   [asChild
                                                      ((genElement (Nothing, "b") []
                                                          [asChild (" "),
                                                           asChild (tpleasemodifname)])),
                                                    asChild (tnamewillappear), asChild (" ")])),
                                             asChild
                                               ((genElement (Nothing, "p")
                                                   [asAttr ("align" := "center")]
                                                   [asChild (" "),
                                                    asChild
                                                      ((genEElement (Nothing, "input")
                                                          [asAttr ("type" := "text"),
                                                           asAttr ("size" := "80"),
                                                           asAttr ("name" := "aname"),
                                                           asAttr ("value" := "")]))])),
                                             asChild (vsep),
                                             asChild ("]\r\n                                   "),
                                             asChild
                                               ((genEElement (Nothing, "input")
                                                   [asAttr ("type" := "hidden"),
                                                    asAttr ("name" := "name"),
                                                    asAttr ("value" := (sname s))])),
                                             asChild
                                               ((genEElement (Nothing, "input")
                                                   [asAttr ("type" := "hidden"),
                                                    asAttr ("name" := "category"),
                                                    asAttr ("value" := (show $ category s))]))])
                              "modify" -> (genElement (Nothing, "p") []
                                             [asChild
                                                ((genEElement (Nothing, "input")
                                                    [asAttr ("type" := "hidden"),
                                                     asAttr ("name" := "name"),
                                                     asAttr ("value" := (sname s))])),
                                              asChild
                                                (if category s == "Constitutional" then
                                                   (genEElement (Nothing, "input")
                                                      [asAttr ("type" := "hidden"),
                                                       asAttr ("name" := "category"),
                                                       asAttr ("value" := "Constitutional")])
                                                   else
                                                   (genElement (Nothing, "p") []
                                                      [asChild
                                                         ((genElement (Nothing, "p")
                                                             [asAttr ("align" := "justify")]
                                                             [asChild (" "),
                                                              asChild
                                                                ((genElement (Nothing, "b") []
                                                                    [asChild (" "),
                                                                     asChild (tpleasecategory),
                                                                     asChild (" ")])),
                                                              asChild (texplaincategory)])),
                                                       asChild
                                                         ([(genElement (Nothing, "p") []
                                                              [asChild
                                                                 ((genEElement (Nothing, "input")
                                                                     [asAttr ("type" := "radio"),
                                                                      asAttr
                                                                        ("checked" :=
                                                                           (show $ x ==
                                                                              category s)),
                                                                      asAttr ("name" := "category"),
                                                                      asAttr
                                                                        ("value" := (show x))])),
                                                               asChild
                                                                 ((genElement (Nothing, "b") []
                                                                     [asChild (show x)]))])
                                                           | x <- lcategories])]))])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "hidden"), asAttr ("name" := "authors"),
                              asAttr ("value" := (listByComma $ authors s))])),
                       asChild
                         (case category s /= "Constitutional" of
                              True -> (genElement (Nothing, "p") []
                                         [asChild
                                            ((genElement (Nothing, "b") []
                                                [asChild (tpleasetopicssub)])),
                                          asChild
                                            ((genElement (Nothing, "p") []
                                                [asChild (topicBoxes)]))])
                              False -> (genEElement (Nothing, "nop") [])),
                       asChild
                         ((genElement (Nothing, "b") [] [asChild (tpleasecontent)])),
                       asChild ((genEElement (Nothing, "br") [])),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "center")]
                             [asChild (contents)])),
                       asChild (vsep),
                       asChild
                         (if category s == "Constitutional" && sname s == majorities then
                            (genEElement (Nothing, "input")
                               [asAttr ("type" := "hidden"), asAttr ("name" := "typeOption"),
                                asAttr ("value" := "approbal")])
                            else
                            (genElement (Nothing, "p") []
                               [asChild
                                  ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                                      [asChild
                                         ((genEElement (Nothing, "input")
                                             [asAttr ("type" := "radio"),
                                              asAttr ("checked" := checkApprobal),
                                              asAttr ("name" := "typeOption"),
                                              asAttr ("value" := "approbal")])),
                                       asChild
                                         ((genElement (Nothing, "b") []
                                             [asChild (tthisisapprobal)])),
                                       asChild (texplainapprobal)])),
                                asChild
                                  ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                                      [asChild
                                         ((genEElement (Nothing, "input")
                                             [asAttr ("type" := "radio"),
                                              asAttr ("name" := "typeOption"),
                                              asAttr ("value" := "choose")])),
                                       asChild
                                         ((genElement (Nothing, "b") [] [asChild (tthisischoose)])),
                                       asChild (texplainchoose)])),
                                asChild
                                  ((genElement (Nothing, "span")
                                      [asAttr
                                         ("style" := "visibility: visible; position: relative")]
                                      [asChild
                                         ((genElement (Nothing, "table") []
                                             [asChild
                                                ((genElement (Nothing, "tr") []
                                                    [asChild (" "),
                                                     asChild
                                                       ((genElement (Nothing, "td")
                                                           [asAttr ("width" := "5%")]
                                                           [asChild (" ")])),
                                                     asChild
                                                       ((genElement (Nothing, "td") [] []))])),
                                              asChild
                                                ((genElement (Nothing, "tr") []
                                                    [asChild (" "),
                                                     asChild ((genElement (Nothing, "td") [] [])),
                                                     asChild
                                                       ((genElement (Nothing, "td") []
                                                           [asChild
                                                              ((genElement (Nothing, "b") []
                                                                  [asChild (tpleasequestion),
                                                                   asChild (" ")]))]))])),
                                              asChild
                                                ((genElement (Nothing, "tr") []
                                                    [asChild (" "),
                                                     asChild ((genElement (Nothing, "td") [] [])),
                                                     asChild
                                                       ((genElement (Nothing, "td") []
                                                           [asChild
                                                              ((genElement (Nothing, "p")
                                                                  [asAttr ("align" := "center")]
                                                                  [asChild
                                                                     ((genElement
                                                                         (Nothing, "textarea")
                                                                         [asAttr
                                                                            ("name" := "question"),
                                                                          asAttr ("rows" := "4"),
                                                                          asAttr ("cols" := "60"),
                                                                          asAttr ("wrap" := "True")]
                                                                         [asChild (question lang s),
                                                                          asChild (" ")]))]))]))])),
                                              asChild
                                                ((genElement (Nothing, "tr") []
                                                    [asChild (" "),
                                                     asChild ((genElement (Nothing, "td") [] [])),
                                                     asChild
                                                       ((genElement (Nothing, "td") []
                                                           [asChild (" "),
                                                            asChild
                                                              ((genElement (Nothing, "p")
                                                                  [asAttr ("align" := "justify")]
                                                                  [asChild (" "),
                                                                   asChild
                                                                     ((genElement (Nothing, "b") []
                                                                         [asChild (" "),
                                                                          asChild
                                                                            (tpleaseoptions)])),
                                                                   asChild (","),
                                                                   asChild (texplainoptions),
                                                                   asChild (" ")])),
                                                            asChild (" ")]))])),
                                              asChild
                                                ((genElement (Nothing, "tr") []
                                                    [asChild (" "),
                                                     asChild ((genElement (Nothing, "td") [] [])),
                                                     asChild
                                                       ((genElement (Nothing, "td") []
                                                           [asChild (" "),
                                                            asChild
                                                              ((genElement (Nothing, "p")
                                                                  [asAttr ("align" := "center")]
                                                                  [asChild
                                                                     ((genElement
                                                                         (Nothing, "textarea")
                                                                         [asAttr
                                                                            ("name" := "options"),
                                                                          asAttr ("rows" := "10"),
                                                                          asAttr ("cols" := "60"),
                                                                          asAttr ("wrap" := "True")]
                                                                         [asChild
                                                                            (if null doptions then
                                                                               texampleoptions else
                                                                               doptions)]))]))]))])),
                                              asChild
                                                ((genElement (Nothing, "tr") []
                                                    [asChild (" "),
                                                     asChild ((genElement (Nothing, "td") [] [])),
                                                     asChild
                                                       ((genElement (Nothing, "td") []
                                                           [asChild (" "),
                                                            asChild
                                                              ((genElement (Nothing, "b") []
                                                                  [asChild (tnoptionstochoose)])),
                                                            asChild
                                                              ((genEElement (Nothing, "input")
                                                                  [asAttr ("type" := "text"),
                                                                   asAttr ("name" := "noptions"),
                                                                   asAttr ("value" := "1")])),
                                                            asChild (tmustbeless)]))]))]))])),
                                asChild (vsep)])),
                       asChild
                         (if sstatus s == Draft then
                            (genElement (Nothing, "p") [asAttr ("align" := "justify")]
                               [asChild (vsep),
                                asChild ((genElement (Nothing, "b") [] [asChild (tstatusofprop)])),
                                asChild (" "),
                                asChild
                                  ((genElement (Nothing, "select") [asAttr ("name" := "status")]
                                      [asChild
                                         ((genElement (Nothing, "option") []
                                             [asChild ("Processing")])),
                                       asChild
                                         ((genElement (Nothing, "option") []
                                             [asChild ("Draft")]))])),
                                asChild (" "), asChild (texplainstatus)])
                            else vsep),
                       asChild
                         ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                             [asChild
                                ((genEElement (Nothing, "input")
                                    [asAttr ("type" := "submit"), asAttr ("name" := "submit"),
                                     asAttr ("value" := tcreatmod)])),
                              asChild
                                ((genEElement (Nothing, "input")
                                    [asAttr ("type" := "reset"), asAttr ("name" := "Cancel"),
                                     asAttr ("value" := tcancel)]))]))]))])
          where {-# LINE 395 "Pages.hs" #-}
                contents
                  = case (content s) of
                        Str str -> (genElement (Nothing, "span") []
                                      [asChild
                                         ((genEElement (Nothing, "input")
                                             [asAttr ("type" := "hidden"),
                                              asAttr ("name" := "contentType"),
                                              asAttr ("value" := "text")])),
                                       asChild
                                         ((genElement (Nothing, "textarea")
                                             [asAttr ("name" := "content"), asAttr ("rows" := "30"),
                                              asAttr ("cols" := "60"), asAttr ("wrap" := "True")]
                                             [asChild (str), asChild (" ")]))])
                        ConfText str -> (genElement (Nothing, "span") []
                                           [asChild
                                              ((genEElement (Nothing, "input")
                                                  [asAttr ("type" := "hidden"),
                                                   asAttr ("name" := "contentType"),
                                                   asAttr ("value" := "Conf")])),
                                            asChild
                                              ((genElement (Nothing, "textarea")
                                                  [asAttr ("name" := "content"),
                                                   asAttr ("rows" := "30"), asAttr ("cols" := "60"),
                                                   asAttr ("wrap" := "True")]
                                                  [asChild (str), asChild (" ")]))])
                {-# LINE 403 "Pages.hs" #-}
                doptions
                  = concatMap (++ "\n") $ takeWhile (/= tcomplaint) $
                      stringOptions lang s
                {-# LINE 405 "Pages.hs" #-}
                topicBoxes
                  = map
                      (\ t ->
                         (genElement (Nothing, "span") []
                            [asChild
                               ((genEElement (Nothing, "input")
                                   [asAttr ("type" := "checkbox"),
                                    asAttr ("name" := ("topic" ++ t)), asAttr ("value" := "OFF"),
                                    asAttr ("checked" := (show $ checked t))])),
                             asChild
                               ((genElement (Nothing, "b") [] [asChild (t), asChild (" ")]))]))
                      (ptopics pr)
                  where {-# LINE 408 "Pages.hs" #-}
                        checked t = elem t (Data.topics s)
                {-# LINE 410 "Pages.hs" #-}
                checkApprobal
                  = case (Data.options s) of
                        Approbal -> "true"
                        _ -> "false"
                {-# LINE 414 "Pages.hs" #-}
                checkChoose
                  = case (Data.options s) of
                        Approbal -> "false"
                        _ -> "true"
        {-# LINE 419 "Pages.hs" #-}
        types = env ->> "type"
        {-# LINE 420 "Pages.hs" #-}
        CalendarTime year month day hour _ _ _ _ _ _ _ _
          = toUTCTime (TOD t1 0)
          where {-# LINE 421 "Pages.hs" #-}
                t1 = lastVote s
        {-# LINE 423 "Pages.hs" #-}
        lang = env ->> acceptLang
        {-# LINE 424 "Pages.hs" #-}
        (tpropcreation, tstatusofprop, tpleaseoptions, tpleasequestion,
         tthisischoose, tthisisapprobal, tpleasecontent, tpleasetopicssub,
         tpleasemodifname, tpleasename, tpleasecategory, tnoptionstochoose,
         tnamewillappear, tmustbeless, texplainstatus, texplainoptions,
         texplainnamesub, explainstatus, texplainchoose, texplaincategory,
         texplainapprobal, teditingsub, tcreatmod, tcancel, tcomplaint,
         texampleoptions)
          = case lang of
                "es" -> (LangEs.propcreation, LangEs.statusofprop,
                         LangEs.pleaseoptions, LangEs.pleasequestion, LangEs.thisischoose,
                         LangEs.thisisapprobal, LangEs.pleasecontent,
                         LangEs.pleasetopicssub, LangEs.pleasemodifname, LangEs.pleasename,
                         LangEs.pleasecategory, LangEs.noptionstochoose,
                         LangEs.namewillappear, LangEs.mustbeless, LangEs.explainstatus,
                         LangEs.explainoptions, LangEs.explainnamesub, LangEs.explainstatus,
                         LangEs.explainchoose, LangEs.explaincategory,
                         LangEs.explainapprobal, LangEs.editingsub, LangEs.creatmod,
                         LangEs.cancel, LangEs.complaint, LangEs.exampleoptions)
                _ -> (Lang.propcreation, Lang.statusofprop, Lang.pleaseoptions,
                      Lang.pleasequestion, Lang.thisischoose, Lang.thisisapprobal,
                      Lang.pleasecontent, Lang.pleasetopicssub, Lang.pleasemodifname,
                      Lang.pleasename, Lang.pleasecategory, Lang.noptionstochoose,
                      Lang.namewillappear, Lang.mustbeless, Lang.explainstatus,
                      Lang.explainoptions, Lang.explainnamesub, Lang.explainstatus,
                      Lang.explainchoose, Lang.explaincategory, Lang.explainapprobal,
                      Lang.editingsub, Lang.creatmod, Lang.cancel, Lang.complaint,
                      Lang.exampleoptions)
{-# LINE 431 "Pages.hs" #-}
validateWidget lang
  = (genElement (Nothing, "form") [asAttr ("action" := cgiURL)]
       [asChild ((genElement (Nothing, "b") [] [asChild (temailt)])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild
          ((genEElement (Nothing, "input")
              [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
               asAttr ("value" := "vor")])),
        asChild
          ((genEElement (Nothing, "input")
              [asAttr ("type" := "text"), asAttr ("size" := "10"),
               asAttr ("name" := "email"), asAttr ("value" := "")])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild ((genElement (Nothing, "b") [] [asChild (tpasswordt)])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild
          ((genEElement (Nothing, "input")
              [asAttr ("type" := "password"), asAttr ("size" := "10"),
               asAttr ("name" := "pass")])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild
          ((genEElement (Nothing, "input")
              [asAttr ("type" := "submit"), asAttr ("name" := "val"),
               asAttr ("value" := tvalidatet)])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild
          ((genElement (Nothing, "a")
              [asAttr ("href" := (cgiURL ++ "?op=regval"))]
              [asChild
                 ((genElement (Nothing, "b") [] [asChild (tregister)]))]))])
  where {-# LINE 447 "Pages.hs" #-}
        (temailt, tpasswordt, tvalidatet, tregister)
          = case lang of
                "es" -> (LangEs.emailt, LangEs.passwordt, LangEs.validatet,
                         LangEs.register)
                _ -> (Lang.emailt, Lang.passwordt, Lang.validatet, Lang.register)
{-# LINE 451 "Pages.hs" #-}
userPage env cookies msg
  = do (projectCookie, env2) <- prCookie env
       page <- if null project then listProjects env2 msg else
                 showProject env
       return Content{mime = page, cookies = (projectCookie ++ cookies)}
  where {-# LINE 459 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 460 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 461 "Pages.hs" #-}
        prCookie env
          = do prs <- if null email then return [] else
                        projectListNames email
               let {-# LINE 464 "Pages.hs" #-}
                   pr = if length prs == 1 && null project then head prs else project
               if not $ null pr then
                 return ([("project", pr, "/", endCookie)], env) else
                 return ([], env)
{-# LINE 471 "Pages.hs" #-}
listProjects env msg
  = do pubpr <- publicProjects
       if email == "" then
         return $ page "freeChooser.com" $ pageBody tabs lang $!
           notLogged pubpr
         else
         do prs <- projectList email
            Just (Ru us) <- getVResource (Ru uUser{Data.name = email})
            return $ page "freechooser.com" $
              pageBody tabs lang (body prs us pubpr)
  where {-# LINE 483 "Pages.hs" #-}
        tabs = createTabs [] Lang.freechooser tyuchus []
        {-# LINE 484 "Pages.hs" #-}
        lang = env ->> acceptLang
        {-# LINE 485 "Pages.hs" #-}
        notLogged pubpr
          = (genElement (Nothing, "p") []
               [asChild (vsep),
                asChild
                  ((genElement (Nothing, "a")
                      [asAttr ("href" := (cgiURL ++ "?op=pro&type=create"))]
                      [asChild (" "),
                       asChild
                         ((genElement (Nothing, "b") []
                             [asChild (" "), asChild (tcreateyourgroup), asChild (" ")]))])),
                asChild (taskacreator), asChild (tyoupublic), asChild (vsep),
                asChild
                  (if null pubpr then (genEElement (Nothing, "br") []) else
                     (genElement (Nothing, "p") []
                        [asChild
                           ((genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
                               [asChild (" "), asChild (tpublicinvitedto),
                                asChild
                                  ((genElement (Nothing, "tr") []
                                      [asChild ((genElement (Nothing, "th") [] [asChild (tnamet)])),
                                       asChild
                                         ((genElement (Nothing, "th") [] [asChild (tauthort)])),
                                       asChild
                                         ((genElement (Nothing, "th") [] [asChild (ttopicst)])),
                                       asChild ((genElement (Nothing, "th") [] [])),
                                       asChild
                                         ((genElement (Nothing, "th") [] [asChild (tactions)]))])),
                                asChild (map (listProject lang blankUser) pubpr)])),
                         asChild ((genElement (Nothing, "h3") [] [asChild (tnogroup)]))]))])
        {-# LINE 506 "Pages.hs" #-}
        body prs us pubpr
          = (genElement (Nothing, "p") []
               [asChild
                  ((genElement (Nothing, "p") [asAttr ("align" := "center")]
                      [asChild (" "),
                       asChild
                         ((genElement (Nothing, "h1") []
                             [asChild (email ++ " " ++ tgroupspage)]))])),
                asChild
                  ((genElement (Nothing, "font")
                      [asAttr ("color" := "FF0000"), asAttr ("size" := "4")]
                      [asChild ((genElement (Nothing, "b") [] [asChild (msg)]))])),
                asChild ((genEElement (Nothing, "br") [])),
                asChild (tthissectionshows),
                asChild
                  (if null prs then
                     (genElement (Nothing, "span") []
                        [asChild ("ddsdfsadf"),
                         asChild
                           ((genElement (Nothing, "h3") [] [asChild (tyouarenotgroup)])),
                         asChild
                           ((genElement (Nothing, "p") []
                               [asChild
                                  ((genElement (Nothing, "a")
                                      [asAttr ("href" := (cgiURL ++ "?op=pro&type=create"))]
                                      [asChild (" "),
                                       asChild
                                         ((genElement (Nothing, "b") []
                                             [asChild (tcreateyourgroup)])),
                                       asChild (" ")])),
                                asChild (taskacreator)])),
                         asChild ((genEElement (Nothing, "br") [])),
                         asChild ((genElement (Nothing, "p") [] [asChild (tyoupublic)]))])
                     else (genEElement (Nothing, "br") [])),
                asChild (listProjects),
                asChild
                  ((genElement (Nothing, "a")
                      [asAttr ("href" := (cgiURL ++ "?op=pro&type=create"))]
                      [asChild
                         ((genElement (Nothing, "b") [] [asChild (tcreategroup)]))])),
                asChild ((genEElement (Nothing, "br") [])),
                asChild
                  ((genElement (Nothing, "h3") []
                      [asChild (tnogroup), asChild (" ")]))])
          where {-# LINE 533 "Pages.hs" #-}
                listProjects
                  = (genElement (Nothing, "p") []
                       [asChild
                          (if null prs then (genEElement (Nothing, "br") []) else
                             (genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
                                [asChild
                                   ((genElement (Nothing, "caption") []
                                       [asChild (tgroupsmemberof), asChild (" ")])),
                                 asChild (header), asChild (map (listProject lang us) prs)])),
                        asChild
                          (if null pubs then (genEElement (Nothing, "br") []) else
                             (genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
                                [asChild
                                   ((genElement (Nothing, "caption") []
                                       [asChild (tpublicinvitedto)])),
                                 asChild (header), asChild (" "),
                                 asChild
                                   ("$\r\n                                                map (listProject lang us) pubs\r\n                                        ")]))])
                {-# LINE 550 "Pages.hs" #-}
                header
                  = (genElement (Nothing, "tr") []
                       [asChild ((genElement (Nothing, "th") [] [asChild (tnamet)])),
                        asChild ((genElement (Nothing, "th") [] [asChild (tauthort)])),
                        asChild ((genElement (Nothing, "th") [] [asChild (ttopicst)])),
                        asChild ((genElement (Nothing, "th") [] [asChild (tdelegate)])),
                        asChild ((genElement (Nothing, "th") [] [asChild (tactions)]))])
                {-# LINE 552 "Pages.hs" #-}
                pubs = pubpr \\ prs
        {-# LINE 555 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 556 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 558 "Pages.hs" #-}
        (tyuchus, tstrListProjects, tyouarenotgroup, tthissectionshows,
         tpublicinvitedto, tyoupublic, tnogroup, tnamet, tgroupspage,
         tgroupsmemberof, tdelegate, tcreateyourgroup, tcreategroup,
         tauthort, ttopicst, tactions, taskacreator)
          = case lang of
                "es" -> (LangEs.yuchus, LangEs.strListProjects,
                         LangEs.youarenotgroup, LangEs.thissectionshows,
                         LangEs.publicinvitedto, LangEs.youpublic, LangEs.nogroup,
                         LangEs.namet, LangEs.groupspage, LangEs.groupsmemberof,
                         LangEs.delegate, LangEs.createyourgroup, LangEs.creategroup,
                         LangEs.authort, LangEs.topicst, LangEs.actions, LangEs.askacreator)
                _ -> (Lang.yuchus, Lang.strListProjects, Lang.youarenotgroup,
                      Lang.thissectionshows, Lang.publicinvitedto, Lang.youpublic,
                      Lang.nogroup, Lang.namet, Lang.groupspage, Lang.groupsmemberof,
                      Lang.delegate, Lang.createyourgroup, Lang.creategroup,
                      Lang.authort, Lang.topicst, Lang.actions, Lang.askacreator)
{-# LINE 564 "Pages.hs" #-}
showProject env
  = do TOD tnow _ <- getClockTime
       Rp pr <- justGetVResource $! Rp uProject{pname = project}
       Ru us <- if null email then return $! Ru blankUser else
                  justGetVResource $! Ru uUser{Data.name = email}
       case (null email, elem email $ users pr, public pr) of
           (True, _, True) -> showp us pr tnow
           (_, True, _) -> showp us pr tnow
           (False, False, True) -> do addUserToProject us pr
                                      showp us pr tnow
           (_, False, False) -> return $ (genEElement (Nothing, "br") [])
       content <- showp us pr tnow
       morep <- moreProjects email
       let {-# LINE 580 "Pages.hs" #-}
           tabs = createTabs [freeChooserName] project "Other projects:" morep
       return $ page project $!
         pageBodyPr tabs (showTextContent $ pdescrip pr) (rigthSide lang)
           content
  where {-# LINE 587 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 588 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 591 "Pages.hs" #-}
        lang = env ->> acceptLang

        {-# LINE 593 "Pages.hs" #-}
        showp :: User -> Project -> Integer -> IO (HSP XML)
        {-# LINE 594 "Pages.hs" #-}
        showp us pr tnow
          = do jprs <- mapM (listSubject lang project email tnow) $
                         psubjects pr
               prs <- return $ catMaybes jprs
               return $
                 (genElement (Nothing, "p") []
                    [asChild
                       ((genElement (Nothing, "b") []
                           [asChild (ttopicst ++ ": "),
                            asChild
                              ((genElement (Nothing, "font")
                                  [asAttr ("color" := "#000080"), asAttr ("size" := "4")]
                                  [asChild
                                     ((genElement (Nothing, "b") []
                                         [asChild (listByComma $ ptopics pr),
                                          asChild (" ")]))]))])),
                     asChild ((genEElement (Nothing, "br") [])),
                     asChild
                       (if pauthor pr /= email then (genEElement (Nothing, "br") []) else
                          (genElement (Nothing, "a")
                             [asAttr
                                ("href" :=
                                   (cgiURL ++ "?op=pro&project=" ++ project ++ "&type=modify"))]
                             [asChild (teditgroup)])),
                     asChild (vsep),
                     asChild
                       (case (psubjects pr) of
                            [] -> (genElement (Nothing, "h2") [] [asChild (tnoproposals)])
                            _ -> (genElement (Nothing, "p") []
                                    [asChild (" "),
                                     asChild
                                       ((genElement (Nothing, "h2") [] [asChild (tsubmittedprop)])),
                                     asChild
                                       ((genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
                                           [asChild (" "),
                                            asChild
                                              ((genElement (Nothing, "caption") []
                                                  [asChild (tproposalspr)])),
                                            asChild
                                              ((genElement (Nothing, "tr") []
                                                  [asChild
                                                     ((genElement (Nothing, "th") []
                                                         [asChild (tnamet)])),
                                                   asChild
                                                     ((genElement (Nothing, "th") []
                                                         [asChild (tauthort)])),
                                                   asChild
                                                     ((genElement (Nothing, "th") []
                                                         [asChild (ttopicst)])),
                                                   asChild
                                                     ((genElement (Nothing, "th") []
                                                         [asChild (tstatust)])),
                                                   asChild
                                                     ((genElement (Nothing, "th") []
                                                         [asChild (tdelegate)])),
                                                   asChild
                                                     ((genElement (Nothing, "th") []
                                                         [asChild (tactions)]))])),
                                            asChild (prs)]))])),
                     asChild
                       ((genElement (Nothing, "a")
                           [asAttr ("href" := (cgiURL ++ "?op=sub&type=create"))]
                           [asChild (" "),
                            asChild
                              ((genElement (Nothing, "b") []
                                  [asChild (" "), asChild (tsubmitnew)]))])),
                     asChild (vsep),
                     asChild
                       ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                           [asChild (" "),
                            asChild ((genElement (Nothing, "b") [] [asChild (ttopicst)])),
                            asChild (" "), asChild (texplaintopicslist)])),
                     asChild
                       (if not $ null email then
                          (genElement (Nothing, "form") [asAttr ("action" := cgiURL)]
                             [asChild
                                ((genEElement (Nothing, "input")
                                    [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
                                     asAttr ("value" := "delegate")])),
                              asChild
                                ((genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
                                    [asChild
                                       ((genElement (Nothing, "caption") [] [asChild (ttopicst)])),
                                     asChild
                                       ((genElement (Nothing, "tr") []
                                           [asChild
                                              ((genElement (Nothing, "th") [] [asChild (ttopic)])),
                                            asChild
                                              ((genElement (Nothing, "th") []
                                                  [asChild (tstrdelegated)])),
                                            asChild
                                              ((genElement (Nothing, "th") []
                                                  [asChild (tdelegatedto)])),
                                            asChild
                                              ((genElement (Nothing, "th") []
                                                  [asChild (tchangedel)]))])),
                                     asChild (listTopics us pr)]))])
                          else (genEElement (Nothing, "br") []))])
        {-# LINE 647 "Pages.hs" #-}
        listTopics us pr = map (showTopicUser us) (ptopics pr)
        {-# LINE 649 "Pages.hs" #-}
        showTopicUser us topic
          = case (find (\ ut -> (uObject ut) == topic) $ usertopics) of
                Just ut -> (genElement (Nothing, "tr") []
                              [asChild (" "), asChild (topic),
                               asChild
                                 ((genElement (Nothing, "td") []
                                     [asChild (show $ length $ delegated ut), asChild (" ")])),
                               asChild
                                 ((genElement (Nothing, "td") [] [asChild (delegatedTo ut)])),
                               asChild (row1), asChild (row2)])
                Nothing -> (genElement (Nothing, "tr") []
                              [asChild (" "),
                               asChild
                                 ((genElement (Nothing, "td") []
                                     [asChild (" "), asChild (topic), asChild (" ")])),
                               asChild
                                 ((genElement (Nothing, "td") []
                                     [asChild (" "), asChild (tnone), asChild (" ")])),
                               asChild
                                 ((genElement (Nothing, "td") []
                                     [asChild (" "), asChild (tnone), asChild (" ")])),
                               asChild (row1), asChild (row2)])
          where {-# LINE 665 "Pages.hs" #-}
                usertopics = utopics us
                {-# LINE 666 "Pages.hs" #-}
                row1
                  = (genElement (Nothing, "td") []
                       [asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("type" := "hidden"), asAttr ("name" := "type"),
                               asAttr ("value" := "topic")])),
                        asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("onfocus" := "this.value=''"), asAttr ("name" := topic),
                               asAttr ("vaue" := tnewdeluser)]))])
                {-# LINE 667 "Pages.hs" #-}
                row2
                  = (genElement (Nothing, "td") []
                       [asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("type" := "submit"), asAttr ("name" := "change"),
                               asAttr ("value" := tchangebut)]))])
        {-# LINE 669 "Pages.hs" #-}
        (tstrproject, ttopic, tsubmitnew, tproposalspr, tsubmittedprop,
         tnoproposals, tnone, tnewdeluser, texplaintopicslist, teditgroup,
         tdontexistdel, tclickherelist, tchangebut, tstrdelegated,
         tdelegatedto, tchangedel, tnamet, tauthort, ttopicst, tstatust,
         tdelegate, tactions)
          = case env ->> acceptLang of
                "es" -> (LangEs.strproject, LangEs.topic, LangEs.submitnew,
                         LangEs.proposalspr, LangEs.submittedprop, LangEs.noproposals,
                         LangEs.none, LangEs.newdeluser, LangEs.explaintopicslist,
                         LangEs.editgroup, LangEs.dontexistdel, LangEs.clickherelist,
                         LangEs.changebut, LangEs.strdelegated, LangEs.delegatedto,
                         LangEs.changedel, LangEs.namet, LangEs.authort, LangEs.topicst,
                         LangEs.statust, LangEs.delegate, LangEs.actions)
                _ -> (Lang.strproject, Lang.topic, Lang.submitnew,
                      Lang.proposalspr, Lang.submittedprop, Lang.noproposals, Lang.none,
                      Lang.newdeluser, Lang.explaintopicslist, Lang.editgroup,
                      Lang.dontexistdel, Lang.clickherelist, LangEs.changebut,
                      Lang.strdelegated, Lang.delegatedto, Lang.changedel, Lang.namet,
                      Lang.authort, Lang.topicst, Lang.statust, Lang.delegate,
                      Lang.actions)
{-# LINE 676 "Pages.hs" #-}
objectUserDel lang us name typeObject
  = case (objectUserDelegate us name typeObject) of
        Nothing -> tnewemail
        Just "" -> tnewemail
        Just del -> del
  where {-# LINE 680 "Pages.hs" #-}
        tnewemail
          = case lang of
                "es" -> LangEs.newemail
                _ -> Lang.newemail
{-# LINE 684 "Pages.hs" #-}
listSubject lang project email tnow subject
  = do Rs s <- justGetVResource $
                 Rs uSubject{prosname = project, sname = subject}
       if sstatus s == Draft && authors s /= [email] then return Nothing
         else
         do Just (Ru us) <- if null email then
                              return $! Just $! Ru $ blankUser else
                              getVResource $! Ru uUser{Data.name = email}
            return . Just $
              (genElement (Nothing, "tr") []
                 [asChild ((genElement (Nothing, "td") [] [asChild (sname s)])),
                  asChild
                    ((genElement (Nothing, "td") []
                        [asChild
                           ((genElement (Nothing, "p") []
                               [asChild
                                  ([(genElement (Nothing, "a")
                                       [asAttr ("href" := ("mailto:" ++ auth))]
                                       [asChild (auth ++ " ")])
                                    | auth <- authors s])]))])),
                  asChild
                    ((genElement (Nothing, "td") []
                        [asChild (listByComma (Data.topics s))])),
                  asChild
                    ((genElement (Nothing, "td") []
                        [asChild (showFinalStatus lang $ sstatus s)])),
                  asChild
                    ((genElement (Nothing, "td") []
                        [asChild
                           (if not $ null email then
                              (genElement (Nothing, "form")
                                 [asAttr ("action" := cgiURL), asAttr ("method" := "post")]
                                 [asChild
                                    ((genEElement (Nothing, "input")
                                        [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
                                         asAttr ("value" := "delegate")])),
                                  asChild
                                    ((genEElement (Nothing, "input")
                                        [asAttr ("type" := "hidden"), asAttr ("name" := "type"),
                                         asAttr ("value" := "subject")])),
                                  asChild
                                    ((genEElement (Nothing, "input")
                                        [asAttr ("type" := "text"),
                                         asAttr ("onfocus" := "this.value=''"),
                                         asAttr ("name" := (subject)),
                                         asAttr
                                           ("value" :=
                                              (objectUserDel lang us subject usubjects))])),
                                  asChild
                                    ((genEElement (Nothing, "input")
                                        [asAttr ("type" := "submit"), asAttr ("name" := "change"),
                                         asAttr ("value" := "change")]))])
                              else (genEElement (Nothing, "span") []))])),
                  asChild
                    ((genElement (Nothing, "td") []
                        [asChild
                           (if canEdit email s (rangev s) then
                              (genElement (Nothing, "p")
                                 [asAttr ("style" := "margin-top: 0; margin-bottom: 0")]
                                 [asChild (editLink $ sname s), asChild (" "),
                                  asChild (delLink project subject),
                                  asChild (viewVoteLink $ sname s)])
                              else viewVoteLink $ sname s)]))])
  where {-# LINE 719 "Pages.hs" #-}
        showFinalStatus lang (Draft)
          = case lang of
                "es" -> (genElement (Nothing, "span") [] [asChild (LangEs.draft)])
                _ -> (genElement (Nothing, "span") [] [asChild (Lang.draft)])
        {-# LINE 722 "Pages.hs" #-}
        showFinalStatus lang (Processing)
          = case lang of
                "es" -> (genElement (Nothing, "span") []
                           [asChild (LangEs.processing)])
                _ -> (genElement (Nothing, "span") [] [asChild (Lang.processing)])
        {-# LINE 726 "Pages.hs" #-}
        showFinalStatus lang (Closed st)
          | lang == "es" =
            (genElement (Nothing, "p") []
               [asChild (" "), asChild (LangEs.tclosed), asChild (" "),
                asChild (showFinalStatus lang st), asChild (" ")])
          | otherwise =
            (genElement (Nothing, "p") []
               [asChild (" "), asChild (Lang.tclosed), asChild (" "),
                asChild (showFinalStatus lang st), asChild (" ")])
        {-# LINE 730 "Pages.hs" #-}
        showFinalStatus lang (Approbed pc)
          | lang == "es" = explain LangEs.approbed LangEs.explApprobed pc
          | otherwise = explain Lang.approbed Lang.explApprobed pc
        {-# LINE 735 "Pages.hs" #-}
        showFinalStatus lang (Rejected (Unconstitutional pc))
          | lang == "es" = explain LangEs.rejected LangEs.explUnconst pc
          | otherwise = explain Lang.rejected Lang.explUnconst pc
        {-# LINE 740 "Pages.hs" #-}
        showFinalStatus lang (Rejected (NegativeVote pc))
          | lang == "es" = explain LangEs.rejected LangEs.explNegativeVote pc
          | otherwise = explain Lang.rejected Lang.explNegativeVote pc
        {-# LINE 744 "Pages.hs" #-}
        showFinalStatus lang (Rejected (NotEnoughVotes pc))
          | lang == "es" =
            explain LangEs.rejected LangEs.explNotEnoughVotes pc
          | otherwise = explain Lang.rejected Lang.explNotEnoughVotes pc
        {-# LINE 748 "Pages.hs" #-}
        explain rej explanation pc
          = (genElement (Nothing, "p") []
               [asChild (rej),
                asChild
                  ((genElement (Nothing, "a")
                      [asAttr
                         ("href" := ("\"javascript:alert('" ++ explanation ++ "')\""))]
                      [asChild (show pc ++ " %")]))])
        {-# LINE 754 "Pages.hs" #-}
        editLink name
          = (genElement (Nothing, "a")
               [asAttr
                  ("href" :=
                     (cgiURL ++ "?op=sub&project=" ++ project ++ "&subject=" ++ name ++
                        "&type=modify"))]
               [asChild (tedit), asChild (" ")])
        {-# LINE 755 "Pages.hs" #-}
        delLink p s
          = (genElement (Nothing, "a")
               [asAttr
                  ("href" :=
                     (cgiURL ++ "?op=del&op2=sub&project=" ++ p ++ "&subject=" ++ s))]
               [asChild (tdelete), asChild (" ")])
        {-# LINE 756 "Pages.hs" #-}
        viewVoteLink name
          = (genElement (Nothing, "a")
               [asAttr
                  ("href" :=
                     (cgiURL ++ "?op=vsub&project=" ++ project ++ "&subject=" ++ name))]
               [asChild (tview), asChild (" ")])
        {-# LINE 757 "Pages.hs" #-}
        rangev s = rangeForVote s tnow
        {-# LINE 759 "Pages.hs" #-}
        (tdelete, tedit, tview)
          = case lang of
                "es" -> (LangEs.delemailt, LangEs.edit, LangEs.view)
                _ -> (Lang.delemailt, Lang.edit, Lang.view)
{-# LINE 763 "Pages.hs" #-}
canEdit email s rangev
  = rangev < daysBefore s && rangev > 0 && [email] == (authors s) &&
      sstatus s
      == Draft
{-# LINE 768 "Pages.hs" #-}
listProject lang us pr
  = (genElement (Nothing, "tr") []
       [asChild ((genElement (Nothing, "td") [] [asChild (nameLink)])),
        asChild ((genElement (Nothing, "td") [] [asChild (authorLink)])),
        asChild
          ((genElement (Nothing, "td") []
              [asChild (listByComma (ptopics pr))])),
        asChild
          ((genElement (Nothing, "td") []
              [asChild
                 (if Data.name us `elem` users pr then
                    (genElement (Nothing, "form") [asAttr ("action" := cgiURL)]
                       [asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
                               asAttr ("value" := "delegate")])),
                        asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("type" := "hidden"), asAttr ("name" := "type"),
                               asAttr ("value" := "project")])),
                        asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("type" := "text"), asAttr ("onfocus" := "this.value=''"),
                               asAttr ("name" := (pname pr)),
                               asAttr
                                 ("value" := (objectUserDel lang us (pname pr) uProjects))])),
                        asChild
                          ((genEElement (Nothing, "input")
                              [asAttr ("type" := "submit"), asAttr ("name" := "change"),
                               asAttr ("value" := "change")]))])
                    else (genEElement (Nothing, "br") []))])),
        asChild
          ((genElement (Nothing, "td") []
              [asChild
                 (if Data.name us == authord then
                    (genElement (Nothing, "p")
                       [asAttr ("style" := "margin-top: 0; margin-bottom: 0")]
                       [asChild (editLink), asChild (" "), asChild (delLink)])
                    else (genEElement (Nothing, "span") [])),
               asChild ("viewLink\r\n                        ")]))])
  where {-# LINE 796 "Pages.hs" #-}
        editLink
          = (genElement (Nothing, "a")
               [asAttr
                  ("href" :=
                     (cgiURL ++ "?op=pro&project=" ++ namep ++ "&type=modify"))]
               [asChild (tedit), asChild (" ")])
              :: HSP XML
        {-# LINE 797 "Pages.hs" #-}
        delLink
          = (genElement (Nothing, "a")
               [asAttr ("href" := (cgiURL ++ "?op=del&op2=pr&project=" ++ namep))]
               [asChild (tdelete)])
              :: HSP XML
        {-# LINE 798 "Pages.hs" #-}
        viewLink
          = (genElement (Nothing, "a")
               [asAttr ("href" := (cgiURL ++ "?project=" ++ namep))]
               [asChild (tselect), asChild (" ")])
              :: HSP XML
        {-# LINE 799 "Pages.hs" #-}
        nameLink
          = (genElement (Nothing, "a")
               [asAttr ("href" := (cgiURL ++ "?project=" ++ namep))]
               [asChild (namep)])
              :: HSP XML
        {-# LINE 800 "Pages.hs" #-}
        authorLink
          = (genElement (Nothing, "a")
               [asAttr ("href" := ("mailto:" ++ authord))]
               [asChild (authord)])
              :: HSP XML
        {-# LINE 801 "Pages.hs" #-}
        namep = pname pr
        {-# LINE 802 "Pages.hs" #-}
        authord = pauthor pr
        {-# LINE 803 "Pages.hs" #-}
        (tdelete, tedit, tselect)
          = case lang of
                "es" -> (LangEs.delete, LangEs.edit, LangEs.select)
                _ -> (Lang.delete, Lang.edit, Lang.select)
{-# LINE 808 "Pages.hs" #-}
anchors lang amends
  = (genElement (Nothing, "p") []
       [asChild
          ((genElement (Nothing, "p") []
              [asChild
                 ((genElement (Nothing, "a") [asAttr ("href" := "#vote")]
                     [asChild (" "),
                      asChild ((genElement (Nothing, "b") [] [asChild (tvote)]))])),
               asChild ((genElement (Nothing, "b") [] [asChild (" / ")])),
               asChild
                 ((genElement (Nothing, "a") [asAttr ("href" := "#delegate")]
                     [asChild
                        ((genElement (Nothing, "b") [] [asChild (tdelegate)]))]))])),
        asChild
          (if null amends then (genEElement (Nothing, "nothing") []) else
             (genElement (Nothing, "p") []
                [asChild
                   ((genElement (Nothing, "a") [asAttr ("href" := "#amendments")]
                       [asChild (" "),
                        asChild
                          ((genElement (Nothing, "b") [] [asChild (tamendments)]))])),
                 asChild (":"), asChild ((genEElement (Nothing, "br") [])),
                 asChild (":\r\n                                 "),
                 asChild
                   ((genElement (Nothing, "ul") []
                       [asChild
                          ([(genElement (Nothing, "li") []
                               [asChild
                                  ((genElement (Nothing, "a") [asAttr ("href" := ("#" ++ a))]
                                      [asChild ((genElement (Nothing, "b") [] [asChild (a)]))])),
                                asChild
                                  (",\r\n                                                        "),
                                asChild
                                  ((genElement (Nothing, "a")
                                      [asAttr ("href" := ("#" ++ a ++ "vote"))]
                                      [asChild
                                         ((genElement (Nothing, "b") [] [asChild (tvote)]))]))])
                            | a <- amends])]))]))])
  where {-# LINE 835 "Pages.hs" #-}
        (tamendments, tvote, tdelegate)
          = case lang of
                "es" -> (LangEs.amendments, LangEs.vote, LangEs.delegate)
                _ -> (Lang.amendments, Lang.vote, Lang.delegate)
{-# LINE 839 "Pages.hs" #-}
showSubject lang s rangev email
  = (genElement (Nothing, "p") []
       [asChild
          ((genElement (Nothing, "p") [asAttr ("align" := "center")]
              [asChild ((genElement (Nothing, "b") [] [asChild (tvvodel)])),
               asChild ((genEElement (Nothing, "br") [])),
               asChild
                 ((genElement (Nothing, "font")
                     [asAttr ("color" := "#000080"), asAttr ("size" := "6")]
                     [asChild (sname s)]))])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild (showSubjectHeader lang s email), asChild (tstatust),
        asChild (" "), asChild (": "),
        asChild
          ((genElement (Nothing, "b") [] [asChild (showStatus s rangev)])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild (showContent lang s), asChild (vsep),
        asChild ((genElement (Nothing, "b") [] [asChild (tdatesvot)])),
        asChild
          ((genElement (Nothing, "table") []
              [asChild
                 ((genElement (Nothing, "tr") []
                     [asChild ((genElement (Nothing, "td") [] [asChild (tinitialvot)])),
                      asChild
                        ((genElement (Nothing, "td") []
                            [asChild ((genElement (Nothing, "b") [] [asChild (startT)]))]))])),
               asChild
                 ((genElement (Nothing, "tr") []
                     [asChild ((genElement (Nothing, "td") [] [asChild (tfinalvot)])),
                      asChild
                        ((genElement (Nothing, "td") []
                            [asChild
                               ((genElement (Nothing, "b") [] [asChild (endT)]))]))]))])),
        asChild (vsep), asChild (tquestionsasked),
        asChild
          ((genElement (Nothing, "p") [asAttr ("align" := "center")]
              [asChild
                 ((genElement (Nothing, "h2") []
                     [asChild (question lang s), asChild (" ")]))]))])
  where {-# LINE 864 "Pages.hs" #-}
        (endT, startT) = subjectTimeParms lang s
        {-# LINE 865 "Pages.hs" #-}
        showStatus s rangev
          | sstatus s == Draft = tdraftexplain
          | rangev == 0 = tundervotation
          | rangev > daysBefore s =
            tacceptingamends ++ ". " ++ show (rangev - daysBefore s) ++
              tdaystovote
          | rangev > 0 =
            tundervotation ++ " " ++ show rangev ++ " " ++ tdaystoend
          | otherwise = tclosed ++ show (- rangev) ++ tdaysago
        {-# LINE 872 "Pages.hs" #-}
        (tstatust, tquestionsasked, tvvodel, tinitialvot, tfinalvot,
         tdraftexplain, tclosed, tdatesvot, tundervotation,
         tacceptingamends, tdaystovote, tdaysago, tdaystoend)
          = case lang of
                "es" -> (LangEs.statust, LangEs.questionsasked, LangEs.vvodel,
                         LangEs.initialvot, LangEs.finalvot, LangEs.draftexplain,
                         LangEs.closed, LangEs.datesvot, LangEs.undervotation,
                         LangEs.acceptingamends, LangEs.daystovote, LangEs.daysago,
                         LangEs.daystoend)
                _ -> (Lang.statust, Lang.questionsasked, Lang.vvodel,
                      Lang.initialvot, Lang.finalvot, Lang.draftexplain, Lang.closed,
                      Lang.datesvot, Lang.undervotation, Lang.acceptingamends,
                      Lang.daystovote, Lang.daysago, Lang.daystoend)
{-# LINE 880 "Pages.hs" #-}
showSubjectHeader lang s email
  = (genElement (Nothing, "span") []
       [asChild (tauthort), asChild (" "), asChild (": "),
        asChild
          ((genElement (Nothing, "b") []
              [asChild ([auth ++ ";" | auth <- authors s]), asChild (" ")])),
        asChild ((genEElement (Nothing, "br") [])), asChild (ttopicst),
        asChild (" "), asChild (": "),
        asChild
          ((genElement (Nothing, "b") []
              [asChild (listByComma (Data.topics s)), asChild (" ")])),
        asChild ((genEElement (Nothing, "br") [])),
        asChild
          (if canEdit email s rangev then editLink s else
             (genEElement (Nothing, "span") []))])
  where {-# LINE 891 "Pages.hs" #-}
        rangev = unsafePerformIO $ subjectRangeVote s
        {-# LINE 892 "Pages.hs" #-}
        editLink s
          = (genElement (Nothing, "a")
               [asAttr
                  ("href" :=
                     (cgiURL ++ "?op=sub&subject=" ++ sname s ++ "&type=modify"))]
               [asChild
                  ("You are the author, edit this proposal\r\n                            ")])
        {-# LINE 895 "Pages.hs" #-}
        (tauthort, ttopicst)
          = case lang of
                "es" -> (LangEs.authort, LangEs.topicst)
                _ -> (Lang.authort, Lang.topicst)
{-# LINE 926 "Pages.hs" #-}
showDiffAmedments amends lang project s email
  = do if null amends then
         return $ (genEElement (Nothing, "nothing") []) else
         do content <- contentIO amends
            return $
              (genElement (Nothing, "p") []
                 [asChild (" "), asChild (header), asChild (" "), asChild (content),
                  asChild (" ")])
  where {-# LINE 933 "Pages.hs" #-}
        ssubject = sname s
        {-# LINE 934 "Pages.hs" #-}
        header
          = (genElement (Nothing, "p") [asAttr ("align" := "center")]
               [asChild
                  ((genElement (Nothing, "h1") [] [asChild (tsuggestedmod)]))])
        {-# LINE 935 "Pages.hs" #-}
        contentIO amends = mapM showDiffAmend amends
        {-# LINE 937 "Pages.hs" #-}
        showDiffAmend name
          = do rangev <- subjectRangeVote s
               Ra a <- justGetVResource $
                         Ra uSubject{aname = name, prosname = project, sname = ssubject}
               Rs s <- justGetVResource $
                         Rs uSubject{sname = ssubject, prosname = project}
               hasVoted <- hasVotedUser email (votes a) project
               return $
                 (genElement (Nothing, "p") []
                    [asChild
                       ((genElement (Nothing, "center") []
                           [asChild
                              ((genEElement (Nothing, "a") [asAttr ("name" := "amendments")])),
                            asChild
                              ((genElement (Nothing, "b") [] [asChild (tamendtoproposal)])),
                            asChild ((genEElement (Nothing, "br") [])),
                            asChild
                              ((genElement (Nothing, "font")
                                  [asAttr ("color" := "#000080"), asAttr ("size" := "6")]
                                  [asChild (aname a)]))])),
                     asChild
                       ((genEElement (Nothing, "a") [asAttr ("name" := (aname a))])),
                     asChild (showSubjectHeader lang a email),
                     asChild
                       ((genElement (Nothing, "h3") []
                           [asChild (" "), asChild ("Content:")])),
                     asChild (" "), asChild (contents s a),
                     asChild
                       ((genElement (Nothing, "h3") []
                           [asChild (" "), asChild ("Topics: ")])),
                     asChild (" "),
                     asChild (change2HTML (topics s) (diff1 (topics s) (topics a))),
                     asChild
                       ((genElement (Nothing, "h3") []
                           [asChild (" "), asChild ("Question:")])),
                     asChild (" "),
                     asChild
                       (change2HTML (strToList $ question lang s)
                          (diff (question lang s) (question lang a))),
                     asChild
                       ((genElement (Nothing, "h3") []
                           [asChild (" "), asChild ("Options: ")])),
                     asChild (" "),
                     asChild
                       (change2HTML (stringOptions lang s)
                          (diff1 (stringOptions lang s) (stringOptions lang a))),
                     asChild
                       (if voteCond s rangev then
                          (genElement (Nothing, "p") []
                             [asChild
                                ((genElement (Nothing, "a")
                                    [asAttr ("name" := (aname a ++ "vote"))]
                                    [asChild
                                       (voteForm1 lang "voteam" name ssubject hasVoted
                                          (amendOptions lang))]))])
                          else (genEElement (Nothing, "span") [])),
                     asChild (votation lang (amendOptions lang) (votes a) (sumVotes a)),
                     asChild (showVoted lang hasVoted (amendOptions lang))])
        {-# LINE 971 "Pages.hs" #-}
        showop op = show op
        {-# LINE 973 "Pages.hs" #-}
        contents s sa
          = case (content s) of
                Str str -> change2HTML (lines1 str) (contDiff sa)
                ConfText str -> change2HTML (lines1 str) (contDiff sa)
        {-# LINE 977 "Pages.hs" #-}
        contDiff a = xs
          where {-# LINE 977 "Pages.hs" #-}
                Changes xs = content a
        {-# LINE 980 "Pages.hs" #-}
        (tamendtoproposal, tsuggestedmod)
          = case lang of
                "es" -> (LangEs.amendtoproposal, LangEs.suggestedmod)
                _ -> (Lang.amendtoproposal, Lang.suggestedmod)
{-# LINE 986 "Pages.hs" #-}
showSubjectPage env
  = do Just (Rs s) <- getVResource $
                        Rs uSubject{prosname = project, sname = subject}
       rangev <- subjectRangeVote s
       moresub <- moreSubjects email
       let {-# LINE 990 "Pages.hs" #-}
           tabs
             = createTabs [freeChooserName] subject "Other proposals:" moresub
       return
         Content{mime = page (sname s) $ pageBody tabs lang $ body s rangev,
                 cookies = []}
  where {-# LINE 996 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 997 "Pages.hs" #-}
        subject = env ->> "subject"
        {-# LINE 998 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 1001 "Pages.hs" #-}
        lang = env ->> acceptLang
        {-# LINE 1002 "Pages.hs" #-}
        body s rangev
          = (genElement (Nothing, "p") []
               [asChild (showSubject lang s rangev email),
                asChild
                  ((genElement (Nothing, "form")
                      [asAttr ("action" := cgiURL), asAttr ("method" := "post")]
                      [asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "submit"), asAttr ("name" := tsendforvote),
                              asAttr ("value" := "")])),
                       asChild
                         ((genEElement (Nothing, "input")
                             [asAttr ("type" := "submit"), asAttr ("name" := tbacktoedit),
                              asAttr ("value" := "")])),
                       asChild (hiddenCodify env)]))])
        {-# LINE 1013 "Pages.hs" #-}
        (tsendforvote, tbacktoedit)
          = case env ->> acceptLang of
                "es" -> (LangEs.sendforvote, LangEs.backtoedit)
                _ -> (Lang.sendforvote, Lang.backtoedit)
{-# LINE 1020 "Pages.hs" #-}
showVoted lang hasVoted options
  = case (hasVoted) of
        Nothing -> (genElement (Nothing, "b") [] [asChild (tnotvoted)])
        Just (Priority pri, IndexVote 0) -> (genElement (Nothing, "b") []
                                               [asChild (tnotvoted)])
        Just (Priority pri, IndexVote v) -> case pri of
                                                4 -> (genElement (Nothing, "p") []
                                                        [asChild
                                                           ((genElement (Nothing, "b") []
                                                               [asChild (tyouvoted)])),
                                                         asChild (votestr v)])
                                                3 -> (genElement (Nothing, "p") []
                                                        [asChild
                                                           ((genElement (Nothing, "b") []
                                                               [asChild (tsubjectvoted)])),
                                                         asChild (votestr v)])
                                                2 -> (genElement (Nothing, "p") []
                                                        [asChild
                                                           ((genElement (Nothing, "b") []
                                                               [asChild (ttopicvoted)])),
                                                         asChild (" "), asChild (votestr v)])
                                                1 -> (genElement (Nothing, "p") []
                                                        [asChild
                                                           ((genElement (Nothing, "b") []
                                                               [asChild (tprojectvoted)])),
                                                         asChild (votestr v)])
  where {-# LINE 1030 "Pages.hs" #-}
        votestr v
          = (genElement (Nothing, "h3") [] [asChild (options !! (v - 1))])
        {-# LINE 1031 "Pages.hs" #-}
        (tsubjectvoted, tyouvoted, tnotvoted, tprojectvoted, ttopicvoted)
          = case lang of
                "es" -> (LangEs.subjectvoted, LangEs.youvoted, LangEs.notvoted,
                         LangEs.projectvoted, LangEs.topicvoted)
                _ -> (Lang.subjectvoted, Lang.youvoted, Lang.notvoted,
                      Lang.projectvoted, Lang.topicvoted)

{-# LINE 1035 "Pages.hs" #-}
votation ::
         String ->
           [String] ->
             DiffArray Int PriorIVote -> DiffUArray Int Int -> HSP XML
{-# LINE 1036 "Pages.hs" #-}
votation lang options votes sumVotes
  = (genElement (Nothing, "center") []
       [asChild
          ((genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
              [asChild
                 ((genElement (Nothing, "caption") []
                     [asChild (tvotationt), asChild (" ")])),
               asChild
                 ((genElement (Nothing, "tr") []
                     [asChild ((genElement (Nothing, "th") [] [asChild (toption)])),
                      asChild ((genElement (Nothing, "th") [] [asChild (tResult)])),
                      asChild ((genElement (Nothing, "th") [] [asChild (tpercent)]))])),
               asChild
                 ([(genElement (Nothing, "tr") []
                      [asChild ((genElement (Nothing, "td") [] [asChild (optioni)])),
                       asChild
                         ((genElement (Nothing, "td") []
                             [asChild
                                ((genElement (Nothing, "b") []
                                    [asChild (printf "%d\n" vi :: String)]))])),
                       asChild
                         ((genElement (Nothing, "td") []
                             [asChild ((printf "%5.2f\n" (percent1 vi)) ++ "%")]))])
                   | (vi, optioni) <- zip v (thasnotvoted : options)])]))])
  where {-# LINE 1050 "Pages.hs" #-}
        v = elems $ sumVotes

        {-# LINE 1051 "Pages.hs" #-}
        percent1 :: Int -> Float
        {-# LINE 1052 "Pages.hs" #-}
        percent1 x = (fromIntegral x * 100) / fromIntegral (upper + 1)
          where {-# LINE 1052 "Pages.hs" #-}
                (0, upper) = bounds votes
        {-# LINE 1053 "Pages.hs" #-}
        (thasnotvoted, tvotationt, toption, tpercent, tResult)
          = case lang of
                "es" -> (LangEs.hasnotvoted, LangEs.votationt, LangEs.option,
                         LangEs.percent, LangEs.result)
                _ -> (Lang.hasnotvoted, Lang.votationt, Lang.option, Lang.percent,
                      Lang.result)
{-# LINE 1059 "Pages.hs" #-}
voteMessage lang s rangev hasVoted
  | rangev == 0 =
    (genElement (Nothing, "b") [] [asChild (tyoucannow)])
  | rangev > daysb =
    (genElement (Nothing, "p") []
       [asChild
          ((genElement (Nothing, "h3") []
              [asChild (show (rangev - daysb) ++ tdaystovote)]))])
  | rangev > 0 =
    (genElement (Nothing, "p") []
       [asChild
          ((genElement (Nothing, "h3") []
              [asChild (voteNow hasVoted s ++ show rangev ++ tdaystoend)]))])
  | otherwise =
    (genElement (Nothing, "h3") []
       [asChild (tclosed ++ show (- rangev) ++ tdaysago)])
  where {-# LINE 1065 "Pages.hs" #-}
        daysb = daysBefore s
        {-# LINE 1067 "Pages.hs" #-}
        voteNow hasVoted s
          = case (hasVoted) of
                Nothing -> tyoucannow
                Just v -> tyoucanchange
        {-# LINE 1071 "Pages.hs" #-}
        (tyoucanchange, tdaystoend, tdaysago, tyoucannow, tdaystovote,
         tclosed)
          = case lang of
                "es" -> (LangEs.youcanchange, LangEs.daystoend, LangEs.daysago,
                         LangEs.youcannow, LangEs.daystovote, LangEs.closed)
                _ -> (Lang.youcanchange, Lang.daystoend, Lang.daysago,
                      Lang.youcannow, Lang.daystovote, Lang.closed)
{-# LINE 1076 "Pages.hs" #-}
showVoteSubjectPage env cookies msg
  = do [Rs s, Rp pr] <- justGetVResources
                          [Rs uSubject{sname = subject, prosname = project},
                           Rp uProject{pname = project}]
       rangev <- subjectRangeVote s
       moresub <- moreSubjects email
       let {-# LINE 1081 "Pages.hs" #-}
           tabs
             = createTabs [freeChooserName, project] subject "Other proposals:"
                 moresub
       if null email then
         return
           Content{mime =
                     page subject $ pageBody tabs lang $
                       showSubject lang s rangev email,
                   cookies = []}
         else
         do hasVoted <- hasVotedUser email (votes s) project
            delegatedTable <- delegates lang project email s
            amends <- getAmends project $ sname s
            amedments <- showDiffAmedments amends lang project s email
            return
              Content{mime =
                        page subject $
                          pageBodyPr tabs (showTextContent $ pdescrip pr) (rigthSide lang)
                          $ body s rangev hasVoted delegatedTable amends amedments,
                      cookies = cookies}
  where {-# LINE 1093 "Pages.hs" #-}
        email = env ->> "email"
        {-# LINE 1094 "Pages.hs" #-}
        subject = env ->> "subject"
        {-# LINE 1095 "Pages.hs" #-}
        project = env ->> "project"
        {-# LINE 1097 "Pages.hs" #-}
        category = env ->> "category"
        {-# LINE 1098 "Pages.hs" #-}
        lang = env ->> acceptLang

        {-# LINE 1100 "Pages.hs" #-}
        body ::
             Subject ->
               Int ->
                 Maybe PriorIVote -> HSP XML -> [String] -> HSP XML -> HSP XML
        {-# LINE 1101 "Pages.hs" #-}
        body s rangev hasVoted delegatedTable amends amedments
          = (genElement (Nothing, "p") []
               [asChild (anchors lang amends),
                asChild
                  ((genElement (Nothing, "font")
                      [asAttr ("color" := "FF0000"), asAttr ("size" := "4")]
                      [asChild ((genElement (Nothing, "b") [] [asChild (msg)]))])),
                asChild
                  (if sstatus s == Draft then showSubject lang s rangev email else
                     (genElement (Nothing, "p") []
                        [asChild (showSubject lang s rangev email), asChild (vsep),
                         asChild (voteMessage lang s rangev hasVoted),
                         asChild
                           (if voteCond s rangev then
                              (genElement (Nothing, "p") []
                                 [asChild
                                    ((genElement (Nothing, "a")
                                        [asAttr
                                           ("href" :=
                                              (cgiURL ++ "?op=sub&subject=" ++ subject ++
                                                 "&type=amend"))]
                                        [asChild
                                           ((genElement (Nothing, "b") []
                                               [asChild (tsuggestmodif)]))])),
                                  asChild (voteForm1 lang "vote" "" subject hasVoted strOpts)])
                              else (genEElement (Nothing, "nothing") [])),
                         asChild (votation lang strOpts (votes s) (sumVotes s)),
                         asChild (showVoted lang hasVoted strOpts), asChild (vsep),
                         asChild ((genElement (Nothing, "h1") [] [asChild (tdelegate)])),
                         asChild
                           ((genElement (Nothing, "p") [asAttr ("align" := "justify")]
                               [asChild (texplaindel), asChild (" ")])),
                         asChild (delegatedTable), asChild (amedments)]))])
          where {-# LINE 1131 "Pages.hs" #-}
                strOpts = stringOptions lang s
        {-# LINE 1134 "Pages.hs" #-}
        delegates lang project email s
          = do Ru us <- justGetVResource $! Ru uUser{Data.name = email}
               return $
                 (genElement (Nothing, "form")
                    [asAttr ("action" := (cgiURL ++ "?op=vsub")),
                     asAttr ("method" := "post")]
                    [asChild
                       ((genElement (Nothing, "p") [asAttr ("align" := "center")]
                           [asChild
                              ((genEElement (Nothing, "a") [asAttr ("name" := "delegate")])),
                            asChild
                              ((genEElement (Nothing, "input")
                                  [asAttr ("type" := "hidden"), asAttr ("name" := "op"),
                                   asAttr ("value" := "delegate")])),
                            asChild
                              ((genElement (Nothing, "table") ([] ++ map asAttr tableStyle)
                                  [asChild
                                     ((genElement (Nothing, "caption") [] [asChild (tdelegatest)])),
                                   asChild
                                     ((genElement (Nothing, "tr") []
                                         [asChild
                                            ((genElement (Nothing, "th") [] [asChild (tdeltype)])),
                                          asChild
                                            ((genElement (Nothing, "th") []
                                                [asChild (tdelemailt)])),
                                          asChild
                                            ((genElement (Nothing, "th") []
                                                [asChild (tchange)]))])),
                                   asChild
                                     ((genElement (Nothing, "tr") []
                                         [asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild
                                                   ((genElement (Nothing, "b") []
                                                       [asChild (tonlyfor ++ tstrproject)]))])),
                                          asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild (" "),
                                                 asChild
                                                   (objectUserDel lang us project uProjects)])),
                                          asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild
                                                   ((genEElement (Nothing, "input")
                                                       [asAttr ("type" := "hidden"),
                                                        asAttr ("name" := "type"),
                                                        asAttr ("value" := "project")])),
                                                 asChild
                                                   ((genEElement (Nothing, "input")
                                                       [asAttr ("type" := "text"),
                                                        asAttr ("onfocus" := "this.value=''"),
                                                        asAttr ("name" := project),
                                                        asAttr ("value" := tnewemail)]))])),
                                          asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild (" "),
                                                 asChild
                                                   ((genEElement (Nothing, "input")
                                                       [asAttr ("type" := "submit"),
                                                        asAttr ("name" := tchangebut),
                                                        asAttr ("value" := tchangebut)])),
                                                 asChild (" ")]))])),
                                   asChild
                                     ((genElement (Nothing, "tr") []
                                         [asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild
                                                   ((genElement (Nothing, "b") []
                                                       [asChild (" "),
                                                        asChild (tonlyfor ++ tstrsubject),
                                                        asChild (" ")])),
                                                 asChild (" ")])),
                                          asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild
                                                   (objectUserDel lang us (sname s) usubjects)])),
                                          asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild
                                                   ((genEElement (Nothing, "input")
                                                       [asAttr ("type" := "hidden"),
                                                        asAttr ("name" := "type"),
                                                        asAttr ("value" := "subject")])),
                                                 asChild
                                                   ((genEElement (Nothing, "input")
                                                       [asAttr ("type" := "text"),
                                                        asAttr ("onfocus" := "this.value=''"),
                                                        asAttr ("name" := (sname s)),
                                                        asAttr ("value" := tnewemail)]))])),
                                          asChild
                                            ((genElement (Nothing, "td") []
                                                [asChild
                                                   ((genEElement (Nothing, "input")
                                                       [asAttr ("type" := "submit"),
                                                        asAttr ("name" := "change"),
                                                        asAttr ("value" := tchangebut)]))]))])),
                                   asChild (map (perTopic us) (Data.topics s))]))]))])
          where {-# LINE 1163 "Pages.hs" #-}
                perTopic us topic
                  = (genElement (Nothing, "tr") []
                       [asChild
                          ((genElement (Nothing, "td") []
                              [asChild (" "),
                               asChild
                                 ((genElement (Nothing, "b") []
                                     [asChild ("Delegated for \"" ++ topic ++ "\""),
                                      asChild (" ")])),
                               asChild
                                 (case (topicDelegatedTo us topic) of
                                      Nothing -> tnone
                                      Just user -> user)])),
                        asChild
                          ((genElement (Nothing, "td") []
                              [asChild
                                 ((genEElement (Nothing, "input")
                                     [asAttr ("type" := "hidden"), asAttr ("name" := "type"),
                                      asAttr ("value" := "topic")])),
                               asChild
                                 ((genEElement (Nothing, "input")
                                     [asAttr ("type" := "text"),
                                      asAttr ("onfocus" := "this.value=''"),
                                      asAttr ("name" := topic),
                                      asAttr ("value" := tnewdeluser)]))])),
                        asChild
                          ((genElement (Nothing, "td") []
                              [asChild
                                 ((genEElement (Nothing, "input")
                                     [asAttr ("type" := "submit"), asAttr ("name" := "change"),
                                      asAttr ("value" := tchangebut)]))]))])
        {-# LINE 1179 "Pages.hs" #-}
        (tsuggestmodif, tstrsubject, tstrproject, tonlyfor, tnone,
         tnewdeluser, tnewemail, texplaindel, tdeltype, tdelemailt,
         tdelegate, tdelegatest, tchangebut, tchange)
          = case env ->> acceptLang of
                "es" -> (LangEs.suggestmodif, LangEs.strsubject, LangEs.strproject,
                         LangEs.onlyfor, LangEs.none, LangEs.newdeluser, LangEs.newemail,
                         LangEs.explaindel, LangEs.delemailt, LangEs.delemailt,
                         LangEs.delegate, LangEs.delegatest, LangEs.changebut,
                         LangEs.change)
                _ -> (Lang.suggestmodif, Lang.strsubject, Lang.strproject,
                      Lang.onlyfor, Lang.none, Lang.newdeluser, Lang.newemail,
                      Lang.explaindel, Lang.delemailt, Lang.delemailt, Lang.delegate,
                      Lang.delegatest, Lang.changebut, Lang.change)
{-# LINE 1186 "Pages.hs" #-}
registrationPage env msg
  = Content{mime = regValHTML env msg, cookies = []}
{-# LINE 1188 "Pages.hs" #-}
validationPage env msg
  = Content{mime = regValHTML env msg, cookies = []}
{-# LINE 1191 "Pages.hs" #-}
regValHTML env msg
  = do page strregval $ pageBody tabs lang $
         (genElement (Nothing, "center") []
            [asChild
               ((genElement (Nothing, "h4") [] [asChild (tyoumustlogin)])),
             asChild
               ((genElement (Nothing, "font")
                   [asAttr ("color" := "FF0000"), asAttr ("size" := "2")]
                   [asChild
                      (case msg of
                           [] -> (genElement (Nothing, "b") [] [asChild (tenteruser)])
                           _ -> (genElement (Nothing, "b") [] [asChild (msg)]))])),
             asChild
               ((genElement (Nothing, "form")
                   [asAttr ("action" := cgiURL), asAttr ("method" := "post")]
                   [asChild
                      ((genElement (Nothing, "table") []
                          [asChild
                             ((genElement (Nothing, "tr") []
                                 [asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genEElement (Nothing, "input")
                                               [asAttr ("type" := "hidden"),
                                                asAttr ("name" := "op"),
                                                asAttr ("value" := "vor")]))]))])),
                           asChild
                             ((genElement (Nothing, "tr") []
                                 [asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genElement (Nothing, "b") [] [asChild (temailt)]))])),
                                  asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genEElement (Nothing, "input")
                                               [asAttr ("type" := "text"),
                                                asAttr ("name" := "email"),
                                                asAttr ("value" := "")]))])),
                                  asChild ((genElement (Nothing, "td") [] []))])),
                           asChild
                             ((genElement (Nothing, "tr") []
                                 [asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genElement (Nothing, "b") []
                                               [asChild (tpasswordt)]))])),
                                  asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genEElement (Nothing, "input")
                                               [asAttr ("type" := "password"),
                                                asAttr ("name" := "pass")]))])),
                                  asChild ((genElement (Nothing, "td") [] [])),
                                  asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genEElement (Nothing, "input")
                                               [asAttr ("type" := "submit"),
                                                asAttr ("name" := "val"),
                                                asAttr ("value" := tvalidatet)]))]))])),
                           asChild (blankRow),
                           asChild
                             ((genElement (Nothing, "tr") []
                                 [asChild
                                    ((genElement (Nothing, "td") [asAttr ("colspan" := "3")]
                                        [asChild
                                           ((genElement (Nothing, "h3") []
                                               [asChild (tenteragain)]))]))])),
                           asChild
                             ((genElement (Nothing, "tr") []
                                 [asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genElement (Nothing, "b") []
                                               [asChild (tpasswordt)]))])),
                                  asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genEElement (Nothing, "input")
                                               [asAttr ("type" := "password"),
                                                asAttr ("name" := "pass2")]))])),
                                  asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genElement (Nothing, "b") [] [asChild (tagainto)]))])),
                                  asChild
                                    ((genElement (Nothing, "td") []
                                        [asChild
                                           ((genEElement (Nothing, "input")
                                               [asAttr ("type" := "submit"),
                                                asAttr ("name" := "reg"),
                                                asAttr ("value" := tregister)]))]))]))])),
                    asChild (hiddenCodify env)]))])
  where {-# LINE 1228 "Pages.hs" #-}
        tabs = createTabs [freeChooserName] strregval "" []
        {-# LINE 1229 "Pages.hs" #-}
        lang = env ->> acceptLang
        {-# LINE 1230 "Pages.hs" #-}
        (strregval, tyoumustlogin, tvalidatet, tregister, tpasswordt,
         tagainto, temailt, tenteragain, tenteruser)
          = case lang of
                "es" -> (LangEs.strregval, LangEs.youmustlogin, LangEs.validatet,
                         LangEs.register, LangEs.passwordt, LangEs.againto, LangEs.emailt,
                         LangEs.enteragain, LangEs.enteruser)
                _ -> (Lang.strregval, Lang.youmustlogin, Lang.validatet,
                      Lang.register, Lang.passwordt, Lang.againto, Lang.emailt,
                      Lang.enteragain, Lang.enteruser)
