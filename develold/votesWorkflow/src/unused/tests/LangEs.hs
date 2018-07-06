module LangEs where

import Data.Map
import System.Locale
acceptLang="Accept-Language"
 
timeLocale= TimeLocale{
			wDays=[("Domingo","Do"),("Lunes", "Lu"),("Martes","Ma"),("Miercoles","Mi"),("Jueves","Ju"),("Viernes","Vi"),
				("Sabado","Sa")]
			,months=[("Enero","Ene"),("Febrero","Feb"),("Marzo","Mar"),("Abril","Abr"),("Mayo","May"),
				("Junio","Jun"),
				("Julio","Jul"),("Agosto","Ago"),("Septiembre","Sep"),("Octubre","Oct"),
				("Noviembre","Nov"),("Diciembre","Dic")]
			,amPm= ("AM","PM")
			,dateTimeFmt= "DD-MM-YYY-hh-mm-ss",dateFmt= "DD-MM-YYYY",timeFmt= "hh-mm-ss",time12Fmt= "hh-mm-ss amPm"
		}

timeFormat 	= "%m-%d-%Y %H:%M:%S WET"


language	= "es"
freechooser	= "www.freeChooser.com"
yuchus		= "Tu elijes"

yourgroups	= "Tus grupos"
proposals	= "Propuestas del grupo"
delegation	= "Delegados"
logout		= "salir"

help		= "Ayuda"
whoweare	= "Quienes somos?"
philosophy	= "Filosofia"
feedback	= "coméntanos"
changelog	= "novedades"

intro2		= "Este sitio Web es una versión preliminar de un sistema de ciber-democracia con muchas capacidades únicas que  unen lo mejor de la democracia directa y la democracia representativa. Estas capacidades permiten cualquier grado de esfuerzo en cada proceso de elección. Tendrás siempre el control de tu voto y de tus representantes porque podrás en cualquier momento revocar su voto y votar por ti mismo. También podrás elegir representantes por cada propuesta a votar, por cada tipo de asunto y globalmente. Las elecciones se extienden durante un largo periodo de tiempo y podrás cambiar tu voto continuamente, por lo que podrás elegír por eliminación una de las dos opciones finalistas. Tu voto es tuyo y cuenta hasta el final. Todo miembro de cada grupo podrá hacer propuestas. Tu eliges. Tu propones. Todo está orientado a permitir a cada miembro el elegir el esfuerzo que desea invertir en las decisiones de grupo, siendo siempre el dueño de su capacidad de decisión. Por qué? porque Internet lo permite"
intro		= "Entra en un grupo o crea el tuyo propio\n- Envía propuestas para discusión y votación\n- Propón cambios a las propuestas de otros\n- Vota directamente o nombra tu representante para el grupo, para el tema o para la propuesta concreta\n- en todo momento puedes modificar el voto de tus representantes\n- Representa tu mismo a otros en votaciones, discusiones etc\n- Consensua cada línea de un acuerdo o haz públicas tus discrepancias y tus apoyos\n- Haz todo eso anónimamente o identificado"

propcreation    = "Creación de Propuesta: "
thispagecollect	= "Esta página recoje información para un grupo de personas que va a tomar una serie de decisiones para ese grupo. El grupo puede ser algo tan pequeño como unos amigos que quieren decidir a donde va en fin de semana hasta todo lo complejo que se quiera."

pleaseentername = "Por favor, introduzca el nombre del grupo. "

explainname	= " Este nombre identificará al grupo."
pleasedescrip	= "Por favor introduzca la descripción del grupo. "
explaindescrip	= "Para estar segudo de que los usuarios del grupo conocen los objectivos, reglas etc, debes expresar todo esto claramente aqui"  

pleasetopics	= "Ahora introduzca los temas que se van a tratar en ese grupo separados por comas. "
explaintopics	= "Los temas son los conceptos clave para las metas del grupo y su sentido debe ser conocido por los miembros del grupo. Los temas sirven para que los miembros delegen si quieren, su decisión para esos temas en otros que tengan parecidos parecidos puntos de vista e intereses pero mayor información. Cada Propuesta que se trate en el grupo debe referirse a uno o varios de los temas que se van a listar aqui"
pleaseemails	= "Por favor, introduzca los identificativos de los miembros. "
explainemails	= " Los miembros podrán participar en las decisiones votando directamente o a través de sus representantes globales, por temas o por cada propuesta. Los miembros también podrán hacer propuestas" 

pleasepublic	= "Active esta opción si quiere que cualquiera interesado pueda unirse al grupo y participar en las decisiones."
explainpublic	= "Posteriormente se podrá expulsar a aquellos que molesten. "
pleasevisible	= "Active esta opción si quiere que el contenido de este grupo sea visble para los no miembros. "
explainvisible	= "Los usuarios externos podrán leer las resoluciones pero no podrán participar."


pleasename	= "Por favor, introduzca el nombre de la propuesta"
explainnamesub	= "Este nombre deberá ser lo suficientemente claro para que lo entiendan los miembros"
pleasetopicssub	= "Seleccione los temas relacionados con la propuesta"
pleasecontent	= "Por favor edite el contenido de la propuesta" 
pleasequestion	= "Introduzca le pregunta genérica a preguntar a los votantes "
pleaseoptions	= "Por favor introduzca las alternativas a votar"

explainoptions	= "Por favor, escriba una opción por línea. Después de una coma, puedes añadir una descripción o un link para explicar cada opción como se ve en el ejemplo" 
exampleoptions	= "ejemplo de opción, ejemplo de texto que describe la opcion 1 \n opción 2 http://www.example.com"
pleaselasttime	= "Por favor introduzca la fecha y hora para el final de la votación"
yeart		= "Año "
montht		= "Mes "
dayt		= "Dia del mes"
hourt		= "Hora "
startvote	= "Empezar la elección "

yes		= "Si"
no		= "No"
complaint	= "Rechazar: Esta propuesta está fuera de los estatutos constitucionales del grupo"

daysbefore	= " dias antes de la fecha y hora final"
startnow	= "Establecer el inicio de la elección ahora"


welcome		= "Bienvenido a freechoser.com"

supportmess	= "mensajes a favor"
sendforvote	= "Enviar para votar"
backtoedit	= "Volver a editar"
youcanalso	= "También puedes:"
suggestmodif	= "Hacer una Enmienda: Sugiere modificaciones para esta propuesta"
hasnotvoted	= "No han votado"


whatis		= "Que es freechoser?"
enteruser	= "Introduzca el usuario y la contraseña. Si entras por primera vez, por favor registrate."
emailt		= "Usuario:"
passwordt	= "Contraseña:"
enteragain	= "Introduzca otra vez la contraseña si quieres registrarte."
againto		= " otra vez para"
register	= "registrarte"
validatet	= "entrar"


nogroup		= "No hay ningun grupo seleccionado"
groupspage	= "grupos del usuario"
askacreator	= " o pide al creador de algun grupo que te incluya"
youpublic	= "Tambien puedes entrar en un grupo público listado aqui:"
thissectionshows= "Esta página presenta los grupos, comunidades y organizaciones a las que tienes acceso. Para el grupo elegido, esta página también muestra las proposiciones que están siendo votadas y los asuntos a los que se refieren. "
youarenotgroup	= "No eres miembro de ningun grupo"
createyourgroup	= "Crea tu propio grupo"
namet		= "Nombre"
authort		= "author"

topicst		= "Asuntos"
actions		= "Acciones"

statusofprop	= "Estado de esta propuesta: "
creatmod	= "Crar/modificar Borrador/Enviar al grupo"
cancel		= "cancelar"

nodata		= "No hay datos para este grupo:"
memberof	= "Eres miembro de: "
--topics		= "Topics "
editgroup	= " Como creador del grupo, puedes editarlo"
noproposals	= "Este grupo no tiene propuestas todavia"
submittedprop	= "Propuestas en este grupo"
submitnew	= "Crea una nueva propuesta"
proposalspr	= "Propuestas existentes"
statust		= "Estado"
creategroup	= "Crear un grupo nuevo"


explaintopicslist= ". Los asuntos son areas de interés a las que las propuestas sometidas a voto se refieren. Puedes delegar tu voto para cualquier propuesta que se refiera a esos asuntos escribiendo en el formulario de abajo, el email de la persona elegida como delegado o representante"

topic		= "Asunto"
usedin		= "Usado en"
strdelegated	= "Votos delegados a ti"
delegatedto	= "Delegado a"
changedel	= "Cambiar delegado"

datesvot	= "Fechas para la elección"	
vvodel		= "Ver/votar/delegar propuesta:"


votethis	= "Vota esta propuesta"
questionsasked	= "Pregunta de la votación:"
initialvot	= "Fecha inicial de la votación"
finalvot	= "Fecha final de la votación"
optionst	= "Opciones a votar:"
vote		= "Vota"
daystovote	= " días para votar"
daystoend	= " días para el final de la votación"
closed		= "Elección ya cerrada hace "
daysago		= " días"
delegate	= "Delega el voto"
explaindel	= "En lugar de votar tu mismo, puede que sea mejor el delegar el voto a alguien con mejor conocimiento y a la vez con intereses y puntos de vista similares a ti para esta propuesta o para los asuntos relacionados con esta propuesta. Hay un rango de prioridades de delegación: El delegado para esta propuesta concreta tiene mayor prioridad que los delegados para los asuntos de los que trata dicha propuesta, y estos tienen mas preferencia que el delegado global por defecto para el grupo, no importa el orden en que voten. Por supuesto tu tienes la maxima prioridad y tu voto cancelará la opcion elegida por tus representantes. Puedes revocar tu voto y darle opcion a tus delegados si votas sin elegir ninguna opción."

votationt	= "Resultados"
notvoted	= "No has votado"
youvoted	= "Tu has votado anteriormente la opción:"
subjectvoted	= "Tu delegado para la propuesta ha votado la opción:"
topicvoted	= "Algun delegado tuyo para los asuntos de esta propuesta ha  votado:"
projectvoted	= "Tu delegado de grupo ha votado la opción:"


delegatest	= "Delegados que te representan en esta propuesta" 
deltype		= "Tipo de delegado"
delemailt		= "Nombre/email de delegado"
change		= "cambiar delegado"
onlyfor		= "Solo para "
strsubject	= "esta propuesta"
strproject	= "este grupo "

youcannow	= "Puedes votar ahora.Quedan "
youcanchange	= "Puedes cambiar tu voto. "
newemail	= "no asignado"
option		= "opción"
result		= "Totales"

edit		= "editar"
delete		= "borrar"

view		= "Ver"
--vote		= "Votar"
--delegate	= "Delegar"
select		= "seleccionar"

younotpermproj	= "Usted no tiene permiso para borrar este proyecto:\""
younotpermsub	= "Usted no tiene permiso para borrar esta propuesta:\""
younotperm	= "Usted no tiene permiso para esta operación"
successdel	= " borrado"

editingsub	= "Editando propuesta: "


morethan	= "El nombre del grupo debe tener mas de 10 caracteres"
succmodgroup 	= "El grupo ha sido modificado con éxito"
grnamenotavail 	= "El nombre elegido ya existe, pruebe otro nombre"	
errorform	= "Hay errores en el formulario. Por favor examine los datos en rojo"
doesnotexist	= "La propuesta no existe, puede haber sido borrada"
succcreagroup	= "El grupo ha sido creado con éxito."
groupmajorities = " Ahora por favor elija los porcentajes de mayorías para cada uno de los siguientes conceptos. Estos porcentajes de mayorías son los datos constitucionales del grupo y cada propuesta estará sujeta a esos criterios. Al mismo tiempo esta es una propuesta de rango constitucional, que estará vigente provisionalmente hasta que acabe el tiempo de votación y estará vigente durante un periodo de tiempo que se define también en esta propuesta. Se recomienda no cambiar los valores si no se conoce muy bien el significado"

succreg		= "Has sido registrado con éxito"   
emaregistered	= "Este usuario ya ha sido registrado, pruebe otro"

loginfail  	= "usuario/password inválido, Por favor introduzca otro usuario/password o registrese"
succval  	= "Bienvenido"

samenamegroup   = "Otra propuesta tiene el mismo nombre, cambielo por favor"
propmorethan	= "El nombre de la propuesta debe tener mas de 10 caracteres. Por favor sea mas descriptivo"

proposalsubmit	= "la propuesta ha side enviada con éxito"
votesuccess	= "Has votado"
notfounddelegate= "datos no encontrado en cambio de delegado"

percent		= "Porcentaje"

nowtxt		= "Ahora"
evertxt		= "A lo largo de la vida del grupo"

pleasecategory	= "Elije el rango de la propuesta."
explaincategory = " Las propuestas constitucionales establecen los estatutos constitutivos del grupo. Las propuestas de largo alcance se refieren a acuerdos que afectan a lo largo de la vida del grupo. Los acuerdos ordinarios son los acuerdos mas comunes para los que el grupo ha sido creado"

agreepercentTxt	= "Porcentaje de votos positivos para validar un acuerdo"
agreeAmendTxt	= "Porcentaje de votos necesarios para validar una enmienda"
complaintTxt	= "Porcentaje de votos necesarios para rechazar una propuesta por anticonstitucional"
newUserTxt	= "Porcentaje de votos necesarios para admitir un nuevo miembro"
necessaryTxt	= "Porcentaje total de votos necesario dar una votación por válida"
forConst	= " en acuerdos de constitución"
forOrdinary	= " para acuerdos ordinarios"
constTimeSpan   = "Vigencia de una propuesta de constitución aprobada"
ordinaryTimeSpan= "Vigencia de una propuesta ordinaria aprobada"

explainapprobal	= "Las propuestas cerradas son aquellas que se aprueban o no por el grupo a  través de votar las opciones \"Si\" o \"No\" ante la pregunta \"Apruebas esta propuesta?\". A pesar de que se llamen cerradas, estas propuestas aceptan modificaciones (enmiendas) que competirán con la original por los votos de los miembros"
			

thisisapprobal	= "Esta es una propuesta cerrada. "
						
thisischoose	= "Esta es una propuesta abierta. "
			
explainchoose	= " Las propuestas abiertas pueden tener una pregunta arbitraria y varias opciones que debes definir en los siguientes entradas de texto:"
			

explainstatus	= "Una propuesta Draft(borrador) no será vista por los otros miembros del grupo, pero tu puedes ver y re-editarla hasta que le cambies el estatus a \"Processing\". Cambia el estatus a \"processing\" cuando estes seguro de que esta lista para votación. Después del envio para votación no sera posible modificarla"
			

notMember	= "(no válido)"
			

approbalquestion= "Estas de acuerdo con esta propuesta?"
			

votationtimeTxt = "Número de días abierto a votación"
			
namewillappear  = "este nombre aparecerá en la lista de modificaciones sugeridas para esta propuesta"
			
pleasemodifname = "Por favor introduzca el nombre de la modificación"
			
noptionstochoose= "número de opciones a elegir"
			
mustbeless	= " debe de ser menos que el número de opciones anteriores"
			
publicinvitedto = "grupos públicos a los que estas también invitado:"
			
groupsmemberof  = "Grupos de los que eres miembro"
			
dontexistdel	= " No existe. Puede que haya sido borrado "
			
clickherelist	= "Pulse aqui para ver la lista de grupos"
			
newdeluser	= "Nombre de delegado nuevo"
			
changebut	= "cambiar"
			
none		= "ninguno"
			
draftexplain	= "Borrador, solo tu ves el documento. Cambia el estatus para enviar la propuesta al grupo"
			
undervotation	= "Bajo votación. "
			
acceptingamends = "en votación y aceptando modificaciones"
			
suggestedmod	= "Modificaciones sugeridas para esta propuesta"
			
amendtoproposal = "Cambios a la propuesta"
			
youmustlogin	= "Debes identificarte para usar esta opción"
			
approbed	= "Aprobado "
draft		= "Borrador "
processing	= "En votación "
tclosed		= "Cerrado "
rejected	= "Rechazado "

explUnconst	= "Un porcentaje suficiente de usuarios ha votado que esta enmienda no es acorde con los objetivos del grupo y sus estatutos "
explNegativeVote= "Un porcentaje suficiente de usuario ha votado en contra de esta propuesta "
explNotEnoughVotes= "No ha habido votos suficientes "
explApprobed	  ="La propuesta ha sido aprobada por este porcentaje de votos"

amendments	= "Modificaciones (enmiendas) para esta propuesta"
strregval       = "identificación de usuario"

strListProjects =       "Grupos"
