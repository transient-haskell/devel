module WebObject
( IWebObject(..), WebObject(..)
, lookupForm, lookupNForm, lookupSForm, lookupNFormObj, lookupSFormObj
, getIntForm, fields, addField, editField2, editField3, editField1
, newFieldName
, substForm
, listParams
) where

import Data.List (find,isPrefixOf)
import Data.Maybe (fromJust, isJust)
import System.IO.Unsafe

import HSP
import HSPClientside
import Data.Maybe(fromMaybe)

import Data(WebObject(..))
import Parameter

import Debug.Trace

debug a b= trace b a

e->>a=  fromMaybe "" (lookup a  e)		--Added
infixr 9 ->>



-- the most usual form objects defined as instances of IWebObject
-- the Form , that main contain many objects is also defined as a IWebObject
-- really the Form here is  a container for the fields that may contain itself . 
-- For this reason, the edit method just create the form fields no  <form> tag is generated. 


data WebObject=   Field  String String
 		| LField String String
		| OField String String
		| WForm String [WebObject] 
		  deriving (Read,Show,Eq) 


{-
addHandler:: String-> PType->PType->PType-> IO ()
addHandler name explain options valida= do
        b <-  addParam explainName explain
 --       if b == True then error $ explainName++ " already defined"
 --        else
        b <- addParam optionsName options
--          if b==True then error $ optionsName++ " already defined" 
--           else
        b <- addParam validaName valida
--            if b==True then error $ validaName++ " already defined" else
        return() 
   where
        explainName= "explain"
        optionsName="options"
        validaName = "validate"
-}		
repl [] _ _ = []
repl xss@(t:xs) s s2 |isPrefixOf s xss	= s2++ repl (drop(length s)xss) s s2
		 	                    |otherwise		= t:repl xs s s2

class IWebObject a where
	-- display the value. the first parameter is the language: "en" "es" "fr" etc
	wDisplay:: String->a->HSP XML

	-- generates the Form input field to edit the object
	edit :: String -> String-> a->HSP XML    	        
	                
	getName :: a -> String

	getValue :: a-> String
	
	hasErr :: a -> Bool
	hasErr a= False
	
	-- get his value from a (name,value) pair list. This list contains the HTML form posted.
	getFromEnv ::  String->a->[(String,String)]-> a

-- optional
    -- show the meaning and display its value
	showExpl:: String-> a->String
    	showExpl lang f= explain lang f++": "++getValue f

	-- validates the object edited. Normaly a callback is used, set previously by addHandlers
	validate:: a-> String -> Maybe String
    	validate w value =  case maybeEvalParam [VString value] ("validate."++name) of
		                        Just (VNothing) -> Nothing
		                        Just (VString s)-> Just s
		                        Nothing -> Nothing
		                        _        -> error $ "invalid validation for "++name
		where name= getName w

	-- return a string that explain the meaning of the field. same as validate: it is set in addHandlers
	explain:: String->a-> String
	explain lang a= case maybeEvalParam [VString lang] ("explain."++name) of
                                        Just(VString str) -> str
                                        Nothing ->  name
	                                _   -> error $  "invalid  validation for " ++ name
	           where name= getName a
        
    -- optional values of the object, to be used in edit. set by addHandlers
	wOptions:: a->[String]
	wOptions a= case maybeEvalParam [VString name] ("options."++name) of
                        Just (VList lst)  -> map(\(VString v)-> v) lst
                        Nothing  -> []
	                _ -> error $ "invalid options for WebObject "++name
	        where name= getName a


instance IWebObject WebObject where
    wDisplay _ f@(Field name value)= 	
        case (hasErr f) of
			False -> <span><%value%></span>
			True  -> <font color="FF0000" size="3"><% value %></font>

    wDisplay _ f@(OField name value)= 	case (hasErr f) of
			False -> <span><% value %></span>
			True  -> <font color="FF0000" size="3"><%  value %></font>

    wDisplay lang (WForm name fields )= <table name=name > <% map showField1 fields %></table> 
      where
	    showField1 f= <field><tr><td><b><%getName f%></b>:  <%explain lang f++":"%></td></tr>
	                         <tr><td><%wDisplay lang f%></td></tr>
	                  </field>

    wDisplay _ (LField name value)= <span><% repl value "\n" "<br/>" %></span> 


    edit lang prefix f@(Field name value)= case (hasErr f) of
				False -> <input type="text" name=name2 value=value />
				True  -> <p><% wDisplay lang f %><p><input type="text" name=name2 value=""/></p></p>
        where name2=name1 prefix name


    edit lang prefix f@(OField name value)= 
          <select name=(name1 prefix name) >
                <option><% value %></option>
                <%[ <option><% op %></option> | op <- wOptions f ] %> 
          </select>

    edit lang prefix f@(WForm name fields )= do
        (tableId, table)<- ref $ <table ><% map (editField2 lang name2) fields %></table>
        <span>
              <%table%>
              <%link1 tableId%>
         </span> 
      where
        link1 tableId= <a style="color:blue"> Add a new parameter to <%name2%><% metaEdit tableId name2 newFieldName fieldAdd %></a>
        name2=name1 prefix name
                 

    edit lang prefix (LField name value)= <textarea name=(name1 prefix name) rows="20" cols="65" wrap="True"><% repl value "\n" "<br/>" %></textarea>
                
                
    hasErr (Field _ v) =  isPrefixOf "Error" v
    hasErr (OField _ v) = isPrefixOf "Error" v
    hasErr (LField _ v) = isPrefixOf "Error" v
    hasErr f@(WForm _ fields)= (not.null $ filter (==True) $ map hasErr fields) || (isJust $ validate f "")


    getFromEnv  prefix f@(Field name _) env= case validate f value of
				Nothing -> Field name value
				Just err-> Field name  err
		 where value= env->>(name1 prefix name)

	
    getFromEnv  prefix f@(OField name _) env= case validate f value of
				Nothing -> OField name value
				Just err-> OField name err
	            where value= env->>(name1 prefix name)
                          
                          
    getFromEnv  prefix f@(LField name _) env= case validate f value of
				Nothing -> LField name value
				Just err-> LField name err
			where value= env->>(name1 prefix name)
			      
			      
    getFromEnv prefix (WForm name fields ) env= WForm name (map (flip (getFromEnv (name1 prefix name)) $ env ) fields) 
                        
    getName (Field n _)=  n
    getName (OField n _)= n
    getName (LField n _)= n
    getName (WForm n _)=  n

    getValue (Field  _ v)= v
    getValue (OField  _ v)= v
    getValue (LField  _ v)= v
    getValue (WForm  n _)= error $ "getValue not implemented  for a form "++n

newFieldName= "Enter_the_name"


  
name1 prefix name | prefix=="" = name | otherwise=prefix++"."++name

editField2 lang prefix f =do 
          (fieldId, field) <- ref $ <span><%editField3 lang prefix f%></span>
          <tr><td width= "5%" />
              <td>
                 <%field%>
                 <%metaEdit fieldId prefix (getName f) fieldPost%>
              </td>
           </tr>

editField1 lang prefix f =  
         <tr><td width= "5%"/>
             <td>
                <%editField3 lang prefix f%>
             </td>
         </tr>

editField3 :: String -> String -> WebObject -> HSP XML 
editField3 lang prefix f =  
        <span>
          <b><%getName f%></b>:  <%explain lang f%><br/>
          <%edit lang prefix  f %>
        </span> 

metaEdit fieldId formName name callback = 
      do
        (panId,panel) <-ref             <div> </div>
        (nameId,namePanel) <-ref        <input value=name />
        (validateId,validatePanel)<-ref <textarea style="width:100%" cols="5"> <%getFormula "validate." name%> </textarea>
        (explainId,explainPanel) <-ref  <textarea style="width:100%" cols="5"> <% getFormula "explain." name%> </textarea> 
        (optionsId,optionsPanel) <-ref  <textarea style="width:100%" cols="5"> <% getFormula "options." name%> </textarea>
        editButRef <- genId
        buttonsId   <- genId
        let reset =  <input type="reset" value="cancel"/> 
                `onClick` do
                                panId # remChildren
                                buttonsId # hideElem
                                editButRef # showElem
 
        let submit= <button type="button"> ok </button>  
                `onClick` do

                        let parms=(    ("name" =: value (toElemNode nameId))
                                   <&> ("oldname" =: name)
                                   <&> ("form" =: formName)
                                   <&> ("validate" =:  ((toElemNode validateId) # firstChild # nodeName)) 
                                   <&> ("options" =:  value (toElemNode optionsId  ))
                                   <&> ("explain" =:  value (toElemNode explainId  ))
                                   )

                        asyncPostReq "http://localhost/fieldPost?" parms (callback fieldId editButRef buttonsId  panId)

        (editId,editPan) <- ref 
                          <p style="margin-left:10%">
                                 <b><%"Editing "++name%> </b><br/>
                                 name<br/>
                                 <% namePanel %><br/><br/>
                                 validate formula<br/>
                                 <% validatePanel %><br/><br/>
                                 explain text <br/>
                                 <% explainPanel %><br/><br/>
                                 options for this field<br/>
                                 <% optionsPanel %><br/><br/> 
                          </p> 
                   
        let editBut= <a id=editButRef style="color:blue">  Edit this parameter</a>  `onClick` do
                        thisElem # hideElem
                        panId # setChild editPan
                        buttonsId  # showElem

        <span><%panel%><%editBut%><span id=buttonsId style="display:none;margin-left:10%"><%submit%><%reset%></span></span>

fieldAdd tableId  editButRef buttonsId panId resp= do
        --let node= resp # responseXML # documentElement    
        let node= resp # domSelFirst "tr"
        let msg= resp # responseText
        alert msg
        alert "adding"
        panId # remChildren
        --table <- panId # getTable
        tableId # appChild node
        buttonsId # hideElem
        editButRef # showElem
              
fieldPost fieldId  editButRef buttonsId panId resp= do
        --let node= resp # responseXML # documentElement    
        let node= resp # domSelFirst "span"
        alert "subst"
        fieldId # setChild node
        panId # remChildren
        buttonsId # hideElem
        editButRef # showElem
              
getTable :: (DomElementRef a b) => a -> HJScript(JsObject TTrue ElementNode)
getTable elem= do
  e <- varWith $  elem # toElemNode
  whileH (e # nodeName .!=. "TABLE") $  
    e .=.  (e  # parentNode)
  return e

whileH :: IsJsBool e b => e -> HJScript () -> HJScript ()
whileH = while    

getFormula prefix parm= case lookupParam (prefix++parm) of
        Nothing -> ""
        Just (Parameter  _) -> "<compiled method>"
        Just (Formula   n _) -> n



fields (WForm name fields)= fields
addField (WForm name fields) f= WForm name (fields++[f])

-- lookup for the value of a parameter in a nested form structure
lookupNFormObj [] o = Just o
lookupNFormObj [x] o | getName o == x = Just o
                     | otherwise= Nothing
                       
lookupNFormObj (x:x1:xs) (WForm name fields)
  |x/=name= Nothing
  |otherwise=
    case lookupFormObj x1  fields  of
    	Nothing  ->	Nothing
    	Just wform  ->	lookupNFormObj xs wform

lokupNFormObj l _ = error $ "lookUp error: "++l 

lookupSFormObj s form= lookupNFormObj ( break1  s) form

break1 s=let 
             bri:: [String]-> String-> String-> [String]
             bri t r ""= t++[r]
	     bri t r (x:xs)| x/='.' = bri t (r++[x]) xs
	                   | otherwise= bri (t++[r]) "" xs 
         in bri [] "" s


         
lookupForm name fields=  
    case  lookupFormObj name fields of
	    Nothing ->		Nothing
	    Just o  ->		Just $ getValue o
	    
lookupNForm xs form=
    case  lookupNFormObj xs form of
	    Nothing ->		Nothing
	    Just o  ->		Just $ getValue o

lookupSForm s form=
    case  lookupSFormObj s form of
	    Nothing ->		Nothing
	    Just o  ->		Just $ getValue o
	    

-- return a webObject by the name from a list
lookupFormObj name fields=  
    (find (\wo-> (getName wo==name)) $ fields) 
 
getIntForm s f=fromIntegral $ read $ fromJust $ lookupForm s f

-- substitution in a Form
substForm:: String ->    -- the path in the format: "formName.formName...." that points to the WO to be substituted in the Form
            WebObject -> -- the new WebObject
            WebObject -> -- the Form that contains the old WO
            WebObject    -- the new Form
substForm path nwo f@(WForm n fields) | path==n = nwo
                                      | otherwise= WForm n $ map (subst2 $ tail $ break1 path ) fields 
                                          
              
 where
         subst2 :: [String]->WebObject->WebObject
         subst2  [x] wo  | getName wo==x = nwo 
                         | otherwise= wo 
         subst2 (x:xs) f@(WForm n fields) | n==x = WForm n $ map (subst2 xs) fields  
                                          | otherwise= f `debug` ("x="++x++" name="++n)
         subst2 _ wo = wo

substForm path nwo wo | path==getName wo = nwo
                      | otherwise= wo
                      
listParams wp= listParams1 "" [wp]           
listParams1  prefix ((WForm name fields):rs)= listParams1 prefix fields ++ listParams1 prefix rs
listParams1  prefix ((Field n v):rs)=(name1 prefix n,v):listParams1 prefix rs 
listParams1  prefix ((LField n v):rs)=(name1 prefix n,v):listParams1 prefix rs 
listParams1  prefix ((OField n v):rs)=(name1 prefix n,v):listParams1 prefix rs
 
