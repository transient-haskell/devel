module FreeChooser.Votes
data Options = Approbal | ChooseOptions String Int [Option] deriving (Read,Show,Eq)
data Option= Option String Status    deriving (Read,Show,Eq)

data Votes= Votes{
  options         :: Options                  -- options to vote
 ,votes           :: DiffArray  Int PriorIVote-- (representant priority, option voted) array
 ,sumVotes        :: DiffUArray Int Int       -- total votes per option
}

data ProjectUsers    :: Map ProjectName [UserName]
data ProjectTopics   :: Map ProjectName [TopicName]
data ProjectSubjects :: ProjectSubjects ProjectName [SubjectName]



data DelegateTypes= Pr ProjectName | Sub SubjectName | Topic TopicName

data Delegations= Delegations{
                oUser            :: UserName
                oProject         :: ProjectName
                ,oType           :: DelegateTypes
                ,delegatedFrom   :: [UserName] 
                ,delegatedTo     :: String
                }deriving (Read,Show,Eq)

data Project= Project{
                pname     :: ProjectName,
                pauthor   :: String,
                users     :: DBRef ProjectUsers
                topics    :: DBRef ProjectTopics
                subjects  :: DBRef ProjectSubjects
                pdescrip  :: String, 
                public    :: Bool,
                visible   :: Bool,
                }deriving (Read,Show)

instance Editable Votes (HSP XML)
   actions _={showline, view,getForm
             ,[showline, view,getForm,delegate]}
             where
             showLine=(Verb "showline", showVoted)
             view=(Verb "view", votation)
             getFrom= (Verb "edit", voteForm)
             delegate=(Verb "delegate", delegatev)


                
showVoted  lang  options =
    hasVoted<- hasVotedUser email (votes a) project 
    case( hasVoted) of
        Nothing         ->  <b><% tnotvoted %></b>
        Just (Priority pri,IndexVote 0)    ->  <b><% tnotvoted %></b>
        Just (Priority pri,IndexVote v)    ->  case pri of
                        4   ->     <p><b><% tyouvoted %></b><% votestr v %></p>
                        3   ->     <p><b><% tsubjectvoted %></b><%votestr v %></p>
                        2   ->     <p><b><% ttopicvoted %></b> <% votestr v %></p>
                        1   ->     <p><b><% tprojectvoted %></b><% votestr v %></p>
    where
    votestr v= <h3><% options !!(v-1) %></h3>
    (tsubjectvoted, tyouvoted,tnotvoted,tprojectvoted,ttopicvoted) = case lang of
            "es" -> (LangEs.subjectvoted, LangEs.youvoted, LangEs.notvoted,LangEs.projectvoted, LangEs.topicvoted)
            _    -> (Lang.subjectvoted, Lang.youvoted, Lang.notvoted,Lang.projectvoted, Lang.topicvoted)

hasVotedUser email svotes project= do
    vote <- voted
    if vote /=(Priority 0, IndexVote 0) then return $ Just vote
     else return Nothing

    where
    voted    = do
                    index <- indexUser email project
                    if index > length  then return (Priority 0,IndexVote 0)
                            else return $ svotes !(index)
    length= u+1 where (0,u)= bounds svotes





votation:: String -> [String] -> DiffArray Int PriorIVote -> DiffUArray Int Int -> HSP XML
votation lang options votes sumVotes= 
        <center> 
                <table tableStyle>
                        <caption><% tvotationt %> </caption>
                        <tr><th><%toption%></th><th><% tResult %></th><th><% tpercent %></th></tr>
                        <%[<tr><td><% optioni %></td><td><b><% printf  "%d\n" vi :: String %></b></td>
                            <td><% (printf  "%5.2f\n" (percent1  vi)) ++"%" %></td>
                           </tr> | (vi,optioni)<- zip v (thasnotvoted:options)
                          ]
                        %>
                </table>
        </center>

        where 
                v= elems $ sumVotes 
                percent1 :: Int -> Float
                percent1 x=  (fromIntegral x*100)/fromIntegral (upper+1)  where (0,upper)= bounds votes
                (thasnotvoted, tvotationt, toption, tpercent, tResult)= case lang of
                            "es" -> (LangEs.hasnotvoted,LangEs.votationt, LangEs.option,LangEs.percent, LangEs.result)
                            _    -> (Lang.hasnotvoted,Lang.votationt, Lang.option,Lang.percent,Lang.result)



voteMessage lang s rangev hasVoted 
        |rangev==0 = <b><% tyoucannow %></b>
        |rangev > daysb = <p><h3><% show (rangev-daysb )++tdaystovote %></h3></p>
        |rangev > 0     = <p><h3><% voteNow hasVoted s++show rangev++tdaystoend %></h3></p>
        |otherwise= <h3><% tclosed++show (-rangev)++tdaysago %></h3>

   where        daysb= daysBefore s

                voteNow hasVoted s =
                     case( hasVoted) of
                        Nothing ->  tyoucannow
                        Just v  ->  tyoucanchange
                (tyoucanchange, tdaystoend,tdaysago,tyoucannow,tdaystovote,tclosed)= case lang of
                                "es" -> (LangEs.youcanchange, LangEs.daystoend,LangEs.daysago, LangEs.youcannow, LangEs.daystovote, LangEs.closed)
                                _    -> (Lang.youcanchange, Lang.daystoend, Lang.daysago, Lang.youcannow, Lang.daystovote, Lang.closed)
