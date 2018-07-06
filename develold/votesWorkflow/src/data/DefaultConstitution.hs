[("OrdinaryProposal", 
   Lambda ( Workf (\action e -> do
     e' <-   action  Propose  e 
     action  (VoteDefault 
              Majorities
               {percentAprobal= 50
       	       ,percentNecessary = 50
	       ,percentComplaint = 10
       	       ,votationTime = 10
	       ,timeSpan= 30}
	      True)  e'

 )))]

