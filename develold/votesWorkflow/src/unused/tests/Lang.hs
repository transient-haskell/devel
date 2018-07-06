module Lang where

import Data.Map as Map
import Control.Concurrent(ThreadId,myThreadId)
import System.IO.Unsafe
import System.Locale

endCookie= "Wed, 31-Dec-2010 12:00:00 GMT" 
transaction     ="transaction"
timeLocale	= defaultTimeLocale
timeFormat 	="%Y-%m-%d %H:%M:%S WET"

cgiURL		= "FreeChooser.cgi"  
language	= "en"

freeChooserName = "FreeChooser.com"
freechooser	= "FreeChooser.com"
yuchus		= "Friction-free democracy: You choose"
			

yourgroups	= "Your groups"
proposals	= "Group proposals"
delegation	= "Delegation"
logout		= "Logout"

help		= "help"
whoweare	= "Who we are?"
philosophy	= "Philosophy"
feedback	= "Feedback"
changelog	= "Change log"

intro2		= "This Web Site is now an alpha version of a public election system with many unique features that make the best of direct and representative democracy, such features permit any degree of involvement on the public decision process. You are ever in control of the vote and the representatives by features such are the long election processes, delegation revocation and reassignment at any time during the election process, publication of partial Results and change of vote in order to better discriminate among many alternatives by elimination of the less voted ones. the user has the option of either direct vote or real-time delegation by different criteria and for any issue to be voted. More features are planned:  The creation of normative,executive and control comissions, formal processes for creation of proposals and amendments and many others. Everithing is oriented to permit each voter to achieve his best balance of effort/control of his public choices"
intro		= "Create your own group or join one\n- Send proposals for discussion and votation\n- Propose amendments to other`s proposals\n- Vote yourself or forget it all about by choosing your representatives by group, discussion or topic at any time\n- Override the decission of your representatives whenever you like\n- Represent many people in votations, discussions and so on\n- Consensuate every line of  agreement documents or make public your discrepancy and the support received\n- Do everithing anonimously or idetified\n"

propcreation    = "Proposal creation: "

thispagecollect	= " This page collect the information for a set of issues to be decided collectively by the people of a group. The group can be something as simple as a (numerous) set of friend that want to decide where to go out on weekend upto something much more complex"	
pleaseentername = "Please enter below the group name: "
explainname	= "The name will identify the group"
pleasedescrip	= "Please enter below the description of the group "
explaindescrip	= "to be sure that the group users know the group objectives, rules etc, you must express them clearly here:"
pleasetopics	= "Now enter the group topics. Enter a few descriptive keywords separated by comma."
explaintopics	= "The topics are key concepts for the group goals that must be well known by the group members. They should be aware about what each topic means in the context of the group, in order to delegate his vote, if they wish, to some other member with appropriate knowledge and similar points of view for the given topic. Every issue to vote in the group must refer to one or many topic of this list"
pleaseemails	= "Please enter the members names  "
explainemails	= "The members can participate in the group decisions either by voting directly or designating delegates for each particular issue, topic etc. They will receive a membership notification by mail (this last functionality is not implemented yet)"
pleasepublic	= "Press this checkbox if you want to permit free participation of anyone interested."
explainpublic	= "you may later ban users by editing the project again" 
pleasevisible	= "Press this checkbox if you want the contents of this group to be readable by other people."
explainvisible	= " External people will read the resolutions, but they could not vote"

pleasename	= "Enter the name of the proposal."
explainnamesub	= " This name must be unambiguous enough to be understood by the group members"
pleasetopicssub	= "In the last case, select the topics that the proposal is related with"
pleasecontent	= "Edit the content of the proposal"
pleasequestion	= "Enter the generic question to ask to the voters"
pleaseoptions	= "Please enter the name of the different alternatives to vote."

explainoptions	= "Write one option per line. You can add a description or a link to a url that explain the option. Don't forget to insert a comma between the option and the explanation text or link, as shown in the example"
exampleoptions	= "example option 1 , example of text describing option 1 \noption 2         , http://www.example.com"
pleaselasttime	= "Please enter the last time-date for the election"
yeart		= "Year "
montht		= "Month "
dayt		= "Day of the month"
hourt		= "Hour "
startvote	= "Start the election "

yes		= "Yes"
no		= "No"
complaint	= "Reject: This proposal is out of the foundational statutes and/or Constitution"
daysbefore	= " days before the end"
startnow	= "Start the election now"
sendforvote	= "Send for votation"
backtoedit	= "back to edit"
supportmess	= "Support messages"
youcanalso	= "You can also:"
suggestmodif	= "Amend: Suggest modifications of this proposal"
hasnotvoted	= "has not voted"


enteruser	= "Enter User/password. If you enter here for the first time, please register"
emailt		= "User:"
passwordt	= "Password:"
enteragain	= "Enter password again if you want to register"
againto		= " again to"
register	= "register"
validatet	= "login"


nogroup		= "No group selected"
groupspage	= "user groups"
askacreator	= " or ask a group creator to include you"
youpublic	= "You can also enter a public group listed below"
thissectionshows= "This page shows the groups, communities and organisations you have access to. For the group selected, this page also show the proposals to vote and the topics that the project refers to"
youarenotgroup	= "You are not involved in any group"
createyourgroup	= "Create your own group"
namet		= "Name"

topicst		= "Topics"
actions		= "Actions"

statusofprop	= "Status of this proposal: "
creatmod	= "Create/modify Draft/Submit to the group"
cancel		= "Cancel"

nodata		= "No data for this group:"
memberof	= "You are member of: "

editgroup	= " You are the owner of this group. you can edit this group"
noproposals	= "This group has no proposals submitted for discussion/vote"
submittedprop	= "Issues submitted for this group"
submitnew	= "Submit a new proposal to the group for election"
proposalspr	= "Submitted proposals"
--namet		= "Name"
authort		= "Author"
statust		= "Status"
--actions		= "Actions"
creategroup	= "Create a new group"


explaintopicslist= ". The topics are areas of interest that the issues submitted for vote are related with. You can delegate the vote for any issue that is relate with a topic by writing, in the form below, the email of the chosen person."

topic		= "Topic"
usedin		= "Used in"
strdelegated	= "Delegated votes"
delegatedto	= "Delegated to"
changedel	= "Change delegate"

datesvot	= "Dates for election"	
vvodel		= "View/vote/delegate proposal:"


--status		= "Status"
votethis	= "Vote this proposal"
questionsasked	= "Question asked for the vote:"
initialvot	= "Initial election date"
finalvot	= "Final election date"
optionst	= "Options to vote:"
vote		= "vote"
daystovote	= " days to vote"
daystoend	= " days to the end"
closed		= "Election closed "
daysago		= " days ago"
delegate	= "Delegate the vote"
explaindel	= "Instead of voting yourself, you can better delegate the vote to some other person with better knowledge and similar interests for this proposal and proposal topics. The delegation type has a priority range: proposal delegates have preference over topic delegates and these have preference over global delegates. The delegate for the first topic of the proposal has preference over the second topic delegate. The vote of the most preferred delegate is the effective one, no matter the chronological order of voting. O course you have the top voting preference, so whenever you vote directly, is your vote the one taken into account"
votationt	= "Results"
notvoted	= "You have not voted"
youvoted	= "You voted before the option:"
subjectvoted	= "Your delegate for this proposal has voted: "
topicvoted	= "Some delegate for the topics of this proposal has voted: "
projectvoted	= "Your delegate for the group has voted: "

delegatest	= "Delegates that represent you in the votation of this proposal" 
deltype		= "delegation type"
delemailt	= "Delegate name/email"
change		= "Change delegate"
onlyfor		= "Only for this "
strsubject	= "proposal"
strproject	= "group"
youcannow	= "You can vote now. "
youcanchange	= "You can change your vote. "
newemail	= "not assigned"
option		= "Option"
result		= "Totals"

edit		= "edit"
delete		= "delete"

view		= "View"
--vote		= "Vote"
--delegate	= "Delegate"
select		= "select"

younotpermproj	= "You have no permission to edit the project:\""
younotpermsub	= "you have no permission to edit the proposal:\""
younotperm	= "You have no permission to do this operation"
successdel	= " Sucessfully deleted"
editingsub	= "Editing proposal: "

morethan	= "The group name has to have more than 10 characters"
succmodgroup 	= "The group was sucessfully modified"
grnamenotavail 	= "The name already exist, please choose another one"
doesnotexist	= "the proposal does not exist. May have been erased"
errorform	= "There are errors in the formulary. Please review the messages in red"	
succcreagroup	= "The group was sucessfully created."
groupmajorities = " Now please chose the percentages of votes for different aspects of group decissions. These values are are the constitutional rules for every group proposal . At the same time this is a proposal itself with constitutional range. This proposal will be provisional until the end of votation for constitutional proposals. Whe approbed, it will be active for a time span for constitutional proposals expressed also here. Afther this period, a new votation period will start and so on. Please keep the current value for each concept if not understood"

succreg		= "You where sucessfully registered"   
emaregistered	= "The user is already registered, please register another user"

loginfail  	= "Login Failed, please re-enter username/password or register"
succval  	= "Welcome"


samenamegroup   = "Another proposal has the same name, please modify it"
propmorethan	= "The proposal name must have more than 10 characters. Please be more descriptive"

proposalsubmit	= "Proposal submitted successfully"
votesuccess	= "You voted sucessfully"
notfounddelegate= "data not found in delegate change"

percent		= "Percent"

nowtxt		= "Now"
evertxt		= "Ever"

pleasecategory	= "Please choose the category of the proposal."
explaincategory = " Constitutional proposals are the ones that stablishes the basic rules of the group. Long reach proposal stablishes important agreements along the life of the group. Ordinary agreements are the common ones for which the group has been created"

agreepercentTxt	= "Percent of positive votes necessary to accept a proposal"
agreeAmendTxt	= "Percent necessary to validate a change in a proposal or agreement"
complaintTxt	= "Percent necessary to reject a proposal as unconstitutional"
newUserTxt	= "Percent of votes necessary to accept or reject a member"
necessaryTxt	= "Percent of total votes necessary to make a votation valid"
forConst	= " with constitutional category"
forOrdinary	= " for ordinary agreements"
constTimeSpan   = "Time span for constitutional proposals approbed"
ordinaryTimeSpan= "Time span for ordinary proposals approbed"

majorities	= "Group majorities"

thisisapprobal	= "This is a closed proposal."
explainapprobal	= " Closed proposals are submitted to be approbed or not by the group trough \"yes\" or \"not\" vote to the question \"Do you approbe this proposal?\". However being named \"closed\" proposals, they may accept modifications (amends) that will contend with the original  for the members votes"
thisischoose	= "This is a open proposal."
explainchoose	= " Open proposals may have arbitrary questions and options to be defined by you in the following boxes:"

explainstatus	= "A Draft proposal will not be seen by others members of the group, but you can see and re-edit her until you change the status to Processing. Change the status to processing when you are sure it is ready to submit for votation. After the submission you will not be able to modify it"

notMember	= "(not valid)"

approbalquestion= "Do you agree with this proposal?"

votationtimeTxt = "Number of days open for votation "
namewillappear  = "This name will appear in the list of suggested modifications for the proposal"
pleasemodifname = "Please enter the modification name."
noptionstochoose= "number of options to choose"
mustbeless	= " must be less than the number of options written above"
publicinvitedto = "Public groups you are invited too:"
groupsmemberof  = "Groups you are member of:"
dontexistdel	= " does not exist. Maybe it has been deleted "
clickherelist	= "Click here to see the list of groups"
newdeluser	= "new delegate name"
changebut	= "change"
none		= "none"
draftexplain	= "Draft, only you see this document. change the status to submit the proposal to the group"
undervotation	= "Under votation"
acceptingamends = "under votation, ccepting amendments"
suggestedmod	= "Suggested modifications to this proposal"
amendtoproposal = "Amend to the proposal"
youmustlogin	= "You must log-in to use this feature"


approbed	= "Approbed "
draft		= "Draft "
processing	= "Processing "
tclosed		= "Closed "
rejected	= "Rejected "

explUnconst	= "A percentage of users has voted it as unconstitutional, that is, he does not meet the objectives and foundational statutes of the group "
explNegativeVote= "A percentage of users has voted against it"
explNotEnoughVotes= "Not enough positive votes"
explApprobed	= "Proposal approbed by this percentage of votes"

amendments	= "changes suggested for this proposal"

strregval       = "User identification"
strListProjects =       "Groups"
