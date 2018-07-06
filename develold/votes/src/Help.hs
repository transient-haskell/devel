{-# OPTIONS -F -pgmFtrhsx -fglasgow-exts   #-}

module Help where
 
import HSP.HTML4
import HSP.Monad
import HSP.XMLGenerator

 
--philosophy :: HSP XML
philosophy  = 
  <p>
       <h1 align="center">freeChooser.com</h1>
	Note: This document is a very early draft
       <h2>Rationale</h2>
       <p>While the projects about online collective decision usually limit themselves
       to facilitate the traditional election processes by means of a strong enphasis
       in voting, security and authentication, the Internet permits much more than
       that. By means of&nbsp; Internet, the physical limitations dissapear.&nbsp; Much
       or the traditional institutions that facilitate collective decissions are the
       Result of physical limitations. That is the case at every level, from the
       procedures of a small community to the institutions of an entire nation. Speakin on communications jargon, 
       a small set of representatives is the consequence of a limited bandwith of communication, a limited processing 
       of information and a limited time.   </p>

	  <p>This is the main cause of the pervasive existence of representatives at all the levels
       whenever collective decision are necessary. This is the cause of the reduced
       set of representatives that conform the legislative, executive and judicial
       institutions in a Democracy. The space-time constraints  impose a reduced set
       of representatives, far and above of the individual voters, that meet phisically togeter in order to achieve
       a reasonable speed on collective decision making.</p>
       
         <p>While traditional public choice institutions are limited by the space-time constraints
        traditional votation systems are also focused on gathering the user choices in a single
        act of votation. The different votation tickets invented trough the history of democracy
        try to get the most of this single event.  </p>

       <p>While most of the citizens may be happy most of the time with these limitations, some
       people, sometimes would like to participate in the decisions, specially in some
       sensible matters in which the party system does not bring to the voter the
       feeling of being represented in its true position. However, the direct
       participation is time consuming and no such effort can be reasonably demanded
       from every citizen. So no practical democracy can rely heavily on&nbsp;
       referenda and other kinds of direct democracy. The same case appears 
       whenever the number of voters are more than few hundreds or whenever the
       issues are so complex that not everyone can dedicate time and effort to be
       informed. </p>
       <p>But Internet could permit the citizen to choose either to participate
       directly or to delegate this activity to a close citizen that the former know has
       the same political attitudes or interests, but with more time, interest and/or information about a certain topic. The later can delegate as well to other, so
       a mix or direct and by representative participation can be possible at the same
       time. Each user can choose other delegate at any time. Both two things permit erase the division between representatives and voters and a qualified representation of any point of view at the voters service. In essence, freeChooser.com is about.&nbsp; While only
       a few voters can have a proper understanding of every issue, it is fairly easy for
       anyone to know other people that have enough knowledge and similar opinions and
       interests for each matter. By redirecting his voice and vote to these representatives on
       each particular issue, the member is better represented.</p>
       <p>Additional tools permits the community member to be informed and involved at
       whatever deep on the collective decision making. The member can, monitor his
       votes, the voting behavior of his representatives and change representatives at
       any time.&nbsp; At any time the member will be in control.</p>
       <h2>What is PCO ?</h2>
       <p>PCO&nbsp; is a system for election processes with optional delegation. Different questions
       on a community are submitted to democratic vote. The questions, about different
       topics may be either be voted directly or the vote may be delegated.</p>
       <p>The goal is to permit the voter to choose either to vote directly all questions
       corresponding with a topic or else to delegate to other person depending on his
       interest, dedication etc. Thus, the users can choose between direct and
       representative democracy by topic or even by each individual question.</p>
       <h2>Complete cycle</h2>
       <p>Although the primary goal of this project is to vote simple issues submitted
       to the system as Result of a deliberation process ouside of the system, a
       process of deliberation in a legislative-like mechanism is foreseen to be
       implemented. The idea is to cover all the cycle from proposition, creation of a
       legislative commission reduced enough, elaboration of amendments, the vote of
       them, the election of a executive to implement the agreed actions&nbsp; and the
       control of the legislative and executive by a separate set of judging delegates
       that may decide about the agreement of the decisions with the community
       constitution or foundational statutes. etc.</p>
       <h3>Election procedure</h3>
       <p>the vote is unrestricted among the members, but, previously to that,&nbsp; in order to
       work on a issue to establish conclusions to be voted, it is necessary to create
       a reduced commission.</p>
       <p>The process of delegation,&nbsp; vote of issues and the nomination of legislative
       and judicial commission and other matters are&nbsp; quite simple, but rather
       different from the traditional processes. The rationale of these differences is
       depicted below:</p>
       <p>To create a legislative commission, it is necessary only to force as rounds of
       delegations in which each user that hasn't received enough votes is forced to
       delegate to other with enough of them for this round until a set of delegates
       reduced enough to work on the elaboration of a law for the chosen question is
       finally elected. The process is similar to a parliamentary election, but the
       flexibility of Internet make it different, simpler and more fair.</p>
       
         <p>For example, to make the election of 10 delegates out from a electoral body of
       1000 electors, it can be arranged in a single round choosing the 10 most voted as
       delegates or , to be fair, to make two rounds permitting the voters to rearrange
       his vote among these 10 nominated delegates in the first round . We may make the
       election more fair with more rounds by publishing the Results of the first round
       and proceed again for a new round, this time with the vote unrestricted. When a
       delegate obtain more than 1000/10 then even if no more votes go to him, he is
       sure delegate. The process continue until the first 10 reach this treshold or a
       timeout is reached. The process in this last case is continuous and does not
       need rounds as such. The advantages are obvious: simpler to implement,
       informative fair and unrestricted. The same mechanism can be used for the
       election of the president of an executive branch or the judicial power.</p>
       
         <p>The rationale above mandates that each user can change his vote at any time, and
       the intermediate votation Results must be visible for all the members.</p>
       <h3>Legislative procedures</h3>
       <p>The legislative procedure starts Once the text is created by the legislative
       commission, an amendment phase can proceed by allowing any voter(including
       delegate) to create an amendment to be accepted or not by the voters. If an
       amendment gain enough votes it is introduced in the final text , ready for the
       final votation. Total amendments can be also allowed, but the details of this
       process are not yet defined. This process does not make use of the legislative
       delegates, but other voting delegates or the direct vote, whatever the voter may
       choose.</p>
       
         <p>In all the processes, the voter may not be involved if he delegates his vote,
       since the delegation operates in cascade: a delegate can delegate its
       corresponding bunch of delegated votes. The delegation may made by topic or by
       individual issue.</p>
       <h3>Control of delegation</h3>
       <p>In order for the voter to control its process of delegation , the voters must
       have the possibility to know the cascade of delegations for his vote and the
       final decision the last delegate made.</p>
       <p>In order to make better choices, a voter may need to know as much as possible
       form a potential delegate. if a potential delegate make public his votes about
       all subjects, the number of delegations for different subjects, then the
       delegate increases the likelihood of being chosen as delegate and becomes&nbsp;
       a &quot;candidate&quot;.</p>
       <p>each issue will have one or more topics, for each topic, each individual
       member can name a delegate if he wish. the delegation will occur at the level of
       topic or at the individual issue level. </p>
       <p>If an issue have more than one topic the user can chose one of them. If the
       user does not choose,&nbsp; the vote is fractionally divided among the delegates
       for each topic..</p>
       <p>topics are assigned to issues from a limited set defined by the foundational
       statutes.</p>
       <h3>Rule of law</h3>
       <p>Other aspect to consider is to establish a constitution/statute for each
       project. Anyone can object that a given disposition voted by majority is not in
       accordance with the constitution/statutes. That may be subject to votation also
       and the rejection will make effective if the votation reach a qualified majory.
       For this reason should be necessary to nominate judicial representatives in the
       initial foundational process that establish the constitution/statute. This
       independent power will have a predefined topic assigned to it.</p>
       <h3>The executive</h3>
       <p>The executive power is composed by a president and a variable set of
       delegates elected by the previously described procedures to be commisioned for&nbsp;
       a particular set of agreed actions . Alternatively, they, can be elected for the
       whole project decisions. That can be up to the project foundational statutes.
       &nbsp;</p>
    </p>

       
