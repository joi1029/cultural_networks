//CONSOLIDATING MULTIPLE VERSIONS OF SAME QUESTIONS
//RECODING VARIABLES FOR USE IN CORRELATIONAL ANALYSIS

set more off

cd "Z:/jc3528/OilSpill/Data"

clear

cap drop _all

clear
clear matrix
clear mata
set maxvar 10000
use gss7224_r1.dta
 
 
*	recode partyid
recode partyid (7=.) (8=.) (9=.)

*	Consolidating multiple versions of same questions

replace natspac=natspacy if natspac>=.
replace natspac=natspacz if natspac>=.

replace natenvir=natenviy if natenvir>=.
replace natenvir=natenviz if natenvir>=.

replace natheal=nathealy if natheal>=.
replace natheal=nathealz if natheal>=.

replace natcity=natcityy if natcity>=.
replace natcity=natcityz if natcity>=.

replace natcrime=natcrimy if natcrime>=.
replace natcrime=natcrimz if natcrime>=.

replace natdrug=natdrugy if natdrug>=.
replace natdrug=natdrugz if natdrug>=.

replace nateduc=nateducy if nateduc>=.
replace nateduc=nateducz if nateduc>=.

replace natrace=natracey if natrace>=.
replace natrace=natracez if natrace>=.

replace natarms=natarmsy if natarms>=.
replace natarms=natarmsz if natarms>=.

replace nataid=nataidy if nataid>=.
replace nataid=nataidz if nataid>=.

replace natfare=natfarey if natfare>=.
replace natfare=natfarez if natfare>=.

replace natroad=natroadz if natroad>=.

replace natmass=natmassz if natmass>=.

replace natsoc=natsocz if natsoc>=.

replace natpark=natparkz if natpark>=.

replace eqwlth=eqwlthy if eqwlth>=.

replace courts=courtsy if courts>=.

replace grass=grassy if grass>=.

replace prayer=prayery if prayer>=.
replace prayer=prayerx if prayer>=.

replace trust = trusty if trust>=.

replace manners=mannersy if manners>=.

replace success=successy if success>=.

replace honest=honesty if honest>=.

replace clean=cleany if clean>=.

replace judgment=judgmeny if judgment>=.

replace control=controly if control>=.

replace role=roley if role>=.

replace amicable=amicably if amicable>=.

replace obeys=obeysy if obeys>=.

replace responsi=responsy if responsi>=.

replace consider=considey if consider>=.

replace interest=interesy if interest>=.

replace studious=studiouy if studious>=.

replace hinumok=hinumoky if hinumok>=.

replace blnumok=blnumoky if blnumok>=.

replace cappun=cappun2 if cappun>=.

replace letdie1=letdie1y if letdie1>=.

replace pillok=pilloky if pillok>=.

replace popespky = 6 - popespky
replace popespks=popespky if popespks>=.

replace polhitok=polhitoy if polhitok>=.

replace aged=agedpar if aged>=.

replace era=eratell if era>=.

replace letin=letin1 if letin>=.

replace tempgen=tempgen1 if tempgen>=.

recode genegood (3=.) (7=.)
replace genegoo1=3-genegoo1
replace genegood=genegoo1 if genegood>=.
replace genegood=genegoo2 if genegood>=.

replace payfam=payfam1 if payfam>=.

replace twoincs=twoincs1 if twoincs>=.


* Post-2016 recoding

* replace marhomo=marsame if marhomo>=.
replace marsame=marsamey if marsame>=.

replace racdif1=racdif1x if racdif1>=.
replace racdif2=racdif2x if racdif2>=.
replace racdif3=racdif3x if racdif3>=.
replace racdif4=racdif4x if racdif4>=.

replace conlabor=conlabory if conlabor>=.

* Volunteered responses -n and -nv (excluded)
* volunteer and no-volunteer weights different, excluded.

* replace uswary = coalesce(uswaryv, uswarynv) if uswary>=.
* replace prayer = coalesce(prayerv, prayernv) if prayer>=.
* replace courts = coalesce(courtsv, courtsnv) if courts>=.
* replace fepol = coalesce(fepolv, fepolnv) if fepol>=.
* replace discaffw = coalesce(discaffwv, discaffwnv) if discaffw>=.
* replace racopen = coalesce(racopenv, racopennv) if racopen>=.
* replace getahead = coalesce(getaheav, getahenv) if getahead>=.
* replace divlaw = coalesce(divlawv, divlawnv) if divlaw>=.
* replace helpful = coalesce(helpfulv, helpfulnv) if helpful>=.
* replace trust = coalesce(trustv, trustnv) if trust>=.
* replace fair = coalesce(fairv, fairnv) if fair>=.
* replace aged = coalesce(agedv, agednv) if aged>=.
* replace grass = coalesce(grassv, grassnv) if grass>=.
* replace reliten = coalesce(relitenv, relitennv) if reliten>=.
* replace bible = coalesce(biblev, biblenv) if bible>=.
* replace postlife = coalesce(postlifv, postlifnv) if postlife>=.
* replace kidssol = coalesce(kidssolv, kidssolnv) if kidssol>=.
* replace uscitzn = coalesce(uscitznv, uscitznnv) if uscitzn>=.
* replace fucitzn = coalesce(fucitznv, fucitznnv) if fucitzn>=.

* -y suffix from gender neutral

replace spkath=spkathy if spkath>=.
replace libath=libathy if libath>=.
replace spkrac=spkracy if spkrac>=.
replace librac=libracy if librac>=.
replace spkcom=spkcomy if spkcom>=.
replace colcom=colcomy if colcom>=.
replace libcom=libcomy if libcom>=.
replace spkmil=spkmily if spkmil>=.
replace libmil=libmily if libmil>=.
replace spkhomo=spkhomoy if spkhomo>=.
replace libhomo=libhomoy if libhomo>=.
replace spkmslm=spkmslmy if spkmslm>=.
replace libmslm=libmslmy if libmslm>=.
replace letdie1=letdie1y if letdie1>=.
replace polhitok=polhitoky if polhitok>=.
replace polabuse=polabusey if polabuse>=.
replace polattak=polattaky if polattak>=.


* replace gridded questions post-2021

replace abdefect=abdefectg if abdefect>=.
replace abhlth=abhlthg if abhlth>=.
replace abnomore=abnomoreg if abnomore>=.
replace abany=abanyg if abany>=.
replace abrape=abrapeg if abrape>=.
replace abpoor=abpoorg if abpoor>=.
replace absingle=absingleg if absingle>=.
replace suicide1=suicide1g if suicide1>=.
replace suicide2=suicide2g if suicide2>=.
replace suicide3=suicide3g if suicide3>=.
replace suicide4=suicide4g if suicide4>=.



// demographics
// describe class
// recode class (5=.) 
// replace class=classy if class >=.

//replace degree = 1 if degree >= 1 & degree <= 4
//replace degree = 0 if degree == 0

recode relig (6=5)(7=5)(8=5)(9=5)(10=5)(11=5)(12=5)(13=5)


*	Recoding some variables to ensure binary, ordinal, or interval measures 
recode colath (4=1) (5=0)
recode colsoc (4=1) (5=0)
recode colrac (4=1) (5=0) 
recode colcom (4=1) (5=0)
recode colmil (4=1) (5=0)
recode colhomo (4=1) (5=0) 
recode colmslm (4=1) (5=0)
recode nafta2 (3=.) (4=.)
recode creation (4=.) (2=5) (3=2)
recode creation (5=3)
recode racopen (2=4) (3=2) 
recode racopen (4=3)
recode racobjct (2=4) (3=2) 
recode racobjct (4=3)
recode racparty (2=4) (3=2) 
recode racparty (4=3)
recode blksimp (2=4) (3=2) 
recode blksimp (4=3)
recode helpful (2=4) (3=2) 
recode helpful (4=3)
recode trust (2=4) (3=2) 
recode trust (4=3)
recode aged (2=4) (3=2) 
recode aged (4=3)
recode agedchld (2=4) (3=2) (4=3)
recode chldidel (8=.)
recode teenpill (3=.)
recode sexeduc (3=.)
recode divlaw (2=4) (3=2) 
recode divlaw (4=3)
recode homosex (5=.)
recode racsubgv (2=4) (3=2) 
recode racsubgv (4=3)
recode defspdr (0=.)
recode hlpminr (0=.)
recode cutspdr (0=.)
recode minmilop (2=4) (3=2) 
recode minmilop (4=3)
recode femilop (2=4) (3=2) 
recode femilop (4=3)
recode genegets (3=.)
recode imppromo (5=.)
recode sexpromo (2=4) (3=2) 
recode sexpromo (4=3)
recode racpromo (2=4) (3=2) 
recode racpromo (4=3)
recode standup (2=4) (3=2) 
recode standup (4=3)
recode opoutcme (3=.)
recode obtohelp (3=.)
recode ethhist (2=4) (3=2) 
recode ethhist (4=3)
recode hmemaker (2=4) (3=2) 
recode hmemaker (4=3)
recode wrkclass (2=4) (3=2) 
recode wrkclass (4=3)
recode manprof (2=4) (3=2) 
recode manprof (4=3)
recode men (2=4) (3=2) 
recode men (4=3)
recode children (2=4) (3=2) 
recode children (4=3)
recode hguncrim (2=4) (3=2) 
recode hguncrim (4=3)
recode socsci (5=.)
recode physcsci (5=.)
recode histsci (5=.)
recode accntsci (5=.)
recode biosci (5=.)
recode econsci (5=.)
recode medsci (5=.)
recode engnrsci (5=.)
recode farming (5=.)
recode journlsm (5=.)
recode fireftng (5=.)
recode marrcoun (5=.)
recode medtreat (5=.)
recode architct (5=.)
recode lawenfrc (5=.)
recode engnring (5=.)
recode slsmnshp (5=.)
recode cmprgmng (5=.)
recode finlcoun (5=.)
recode gunsdrug (2=4) (3=2) 
recode gunsdrug (4=3)
recode moretrde (4=.)
recode fair5 (6=.)
recode unrghts (3=.)

//Keep variables that will be used for correlational analysis

keep year partyid polviews feelblks feelasns feelhsps feelwhts natspac natenvir natheal natcity natcrime natdrug nateduc natrace natarms nataid natfare natroad natsoc natmass natpark natchld natsci natenrgy equal1 equal2 equal3 equal4 equal5 equal6 equal7 equal8 usclass1 usclass2 usclass3 usclass4 usclass5 usclass6 usclass7 usclass8 educop govcare eqwlth spkath colath libath spksoc colsoc libsoc spkrac colrac librac spkcom colcom libcom spkmil colmil libmil spkhomo colhomo libhomo spkmslm colmslm libmslm cappun gunlaw courts wirtap grass prayer libtemp contemp prottemp cathtemp jewtemp mslmtemp fepriest feclergy racmar racdin racpush racseg racopen racobjct racschol racfew rachaf racmost busing racpres racjob racnobuy racparty racocc racinc affrmact wrkwayup blksimp helpful trust confinan conbus conclerg coneduc confed conlabor conpress conmedic contv conjudge consci conlegis conarmy manners success honest clean judgment control role amicable obeys responsi consider interest studious obey popular thnkself workhard helpoth youngen aged agedchld anomia1 anomia2 anomia3 anomia4 anomia5 anomia6 anomia7 anomia8 anomia9 fehome fework fepres fepol abdefect abnomore abhlth abpoor abrape absingle abany chldidel pill teenpill pillok sexeduc divlaw premarsx teensex xmarsex homosex homochng porninf pornmorl pornrape pornout pornlaw spanking letdie1 suicide1 suicide2 suicide3 suicide4 hitok hitmarch hitdrunk hitchild hitbeatr hitrobbr polhitok polabuse polmurdr polescap polattak abspno abhave1 abhave2 abhave3 ablegal fechld fehelp fepresch fefam era febear feworkif racsubs racsubgv racmarpr racsups racteach racdif1 racdif2 racdif3 racdif4 defspdr hlpminr cutspdr impfam impwork imprelax impfrend impkin impchurh imppol privacy pollgood feserve meserve taxserve milqual milpay fenumok hinumok blnumok milvolok fightair mechanic nurse typist brass fightlnd transair gunner fightsea fehlpmil draft draftcol draftmar draftpar draftgay draftco draftdef minmilop femilop copunish cojail milokme milokfe upgrade jobtrain obvote obvol objury ob911 obeng obknow obmepax obmewar obfepax obfewar helppoor helpnot helpsick helpblk fedtrust blkgains fegains scisolve scichng scipry scimoral god punsin blkwhite rotapple permoral decbible decoths decchurh decself gochurch believe follow goownway godsells godsport implives obeytch ownthing talkback twoclass openmind whypoor1 whypoor2 whypoor3 whypoor4 socdif1 socdif2 socdif3 socdif4 wlthwhts wlthjews wlthblks wlthasns wlthhsps workwhts workjews workblks workasns workhsps workso violwhts violjews violblks violasns violhsps violso intlwhts intljews intlblks intlasns intlhsps intlso farewhts farejews fareblks fareasns farehsps fareso patrwhts patrjews patrblks patrasns patrhsps patrso workfare lessfare povzone povschs povcol blkzone blkschs blkcol racquota influwht influjew influblk influasn influhsp influso hspjobs blkjobs asnjobs hsphouse blkhouse asnhouse discaff genejob genehire genecanx genecany genegets profits1 profits2 unpower unprog imppromo sexpromo racpromo standup selfirst richpoor opoutcme united obtohelp lfegod lfegenes lfesocty lfehrdwk lfechnce bigband blugrass country blues musicals classicl folk gospel jazz latin moodeasy newage opera rap reggae conrock oldies hvymetal judgeart trstprof classics grtbooks modpaint english pclit excelart hosthome impfinan impmar impkids impgod impthngs impcultr impjob impself amrank amproud meltpot gvtapart gvtmelt ethorgs ethspkok ethspkno symptblk admirblk bilinged engteach engballt engoffcl letin immecon immunemp immunite immfare undocwrk undoccol undockid immpush immwrkup colaff colaffy discaffy jobaff owneth congeth teacheth schleth ethhist whoteach whtgovt blkgovt hspgovt asngovt wlthimm wlthundc workimm workundc obrespct fejobaff discaffm discaffw flextime parleave menben womenben chldben allben nooneben menhrt womenhrt chldhrt allhrt noonehrt feless1 feless2 feless3 fekids1 fekids2 fekids3 fekids4 fekids5 mebear fehire feminist fenews hmemaker wrkclass manprof men children frnddeal frndawk tablprce sellorgn sellbaby sellsex reqinfo natrecon econsys  spmentl govmentl  godwatch lesspain natarts artgod artists irrelart irreloff aimofart natlart stateart localart trusting psycmed1 psycmed2 psycmed3 psycmed4 psycmed5 psycmed6 psycmed7 hmo1 hmo2 hmo3 hmo4 hmo5 hmo6 hmo7 doc1 doc2 doc3 doc4 doc5 doc7 doc8 doc10 proz1 proz2 proz3 proz4 proz5 proz6 proz7 proz8 proz9 socsecrt socsecfx socsecnu famwhts famblks famjews famhsps famasns fairwhts fairblks fairjews fairhsps fairasns conteng contitl contchn contjew contblk contmex contvn contcuba contirsh contpr contjpn contmslm engoff1 twolang nobiling engunite forlang1 engthrtn engvote letinhsp letinasn letineur immcrmup immnew immnojob howfree freenow leftlone nogovt inpeace partpol choice expunpop wlthpov ethignor ethnofit ethtrads ethadapt ethsame ethdiff whtsdiff trdunion othshelp careself selffrst finind ownhh eddone ftwork supfam havchld getmar outsider sufadult comknows failure ovrmedkd medkdneg putsoff trbllaw medsavtx zombies pryntfam adhdreal adhdcon adhdmed medsymps medaddct medweak medunacc geneexps hgunlaw hguncrim idols rosaries notthink nextgen toofast bettrlfe advfront scispec leadsci whichsci astrosci scibnfts balpos balneg gwsci gwpol gwbiz sciagrgw sciinfgw polinfgw bizinfgw scibstgw polbstgw bizbstgw scmed screlig scpol medagrsc medinfsc relinfsc polinfsc medbstsc relbstsc polbstsc scresrch txeco txbiz txpol ecoagree ecoinftx bizinftx polinftx ecobsttx bizbsttx polbsttx gmmed gmpol gmbiz medagrgm medinfgm polinfgm bizinfgm medbstgm polbstgm bizbstgm sciimp1 sciimp2 sciimp3 sciimp4 sciimp5 sciimp6 sciimp7 sciimp8 socsci physcsci histsci accntsci biosci econsci medsci engnrsci comorsci nanowill nanoben nanoharm scimath anscitst morempg polnuke biznuke engnuke engagrnk enhinfnk polinfnk bizinfnk engbstnk polbstnk bizbstnk nukeelec sciental scientdn scientgo scientfu scienthe scientod scientbe scientre scientwk scientmo scientbr englone engdgr enggood engfun engprob engodd engbtr engrel engint engearn engbrng farming journlsm fireftng marrcoun medtreat architct lawenfrc engnring slsmnshp cmprgmng finlcoun gunsales gunsdrug semiguns guns911 rifles50 gunsdrnk spnatdis natdisin natdiscm getaheay mhfright mhsymp moretrde polefy3 polefy11 polefy13 polefy15 polefy16 polefy17 pubdef pubecon obeylaw protest1 protest2 protest3 protest4 protest5 protest6 revspeak revtch15 revpub racspeak ractch15 racpub crimtail crimtap crimread crimhold mantail mantap manread manhold verdict databank progtax eqincome oprich opprof opfamily fecolop fejobop feinc fehlpbus fehlpcol fehlpjob hsbasics hssexed hsrespct hslibart hsjudge hsjobtr hssci hscaring hsorder colop aidneedy aidsmart aidavg kiddrugs kidskips kidout kidneedy kidbeat kidhlth kidedpar kidxfilm beltup nosmoke mustret poleff1 poleff2 poleff3 poleff4 poleff6 poleff7 poleff9 poleff10 setwage setprice cutgovt makejobs lessreg hlphitec savejobs cuthours spenviro sphlth sppolice spschool sparms spretire spunemp sparts bustax infljobs laborpow buspow govtpow ownpower ownmass ownsteel ownbanks ownautos jobsall pricecon hlthcare aidold aidindus aidunemp equalize aidcol aidhouse protstrs revoltrs racists unionsok grnlaws gendereq poleff12 poleff14 poleff16 poleff17 demworks taxspend taxpaid taxbylaw brlawfl brnotax runpower runhosp runbanks cutdebt helphlth helpold helpsec helpcrim helpemp helpenv cctv emonitor govtinfo givinfusa givinffor wotrial tapphone stoprndm knowpols corrupt1 corrupt2 opwlth oppared opeduc opambit opable ophrdwrk opknow opclout oprace oprelig opregion opsex oppol incentiv inequal1 inequal2 inequal3 inequal4 inequal5 inequal6 inequal7 incgap goveqinc govedop govjobs govless govunemp govminc taxrich taxmid taxpoor taxshare conwlth conclass conjobs conunion conurban consoc conage rewrdeff rewrdint corrupt ldcgap ldctax richhlth richeduc payresp payedtrn paysup paychild paydowel payhard mawrkwrm kidsuffr famsuffr hapifwrk homekid housewrk fejobind twoincs hubbywrk wrknokid wrkbaby wrksch wrkgrown daycare1 daycare2 daycare3 daycare4 daycare5 femarry memarry marhappy marfree marfin markids marnomar marlegit marmakid marpakid mardiv kidtrble kidjoy kidnofre kidless kidfin kidempty divnokid divifkid divkids divwife divhubby fewrksup hubbywk1 mrmom meovrwrk singlpar cohabok cohabfst divbest divifkd1 divnokd1 mapaid chldcare abchoose teensex1 mehhwork mekdcare fewknokd ssfchild ssmchild kidfinbu kidjob kidsocst eldersup paidlv wrkearn wrkimp yrsfirm dowell expernc paysame ageemp sexemp famresp educemp bosswrks strngun unionsbd paydojob payfam payeduc paytime techjobs techwork selfemp1 selfemp2 unjobsec unbetter stiffpun deathpen premars1 xmarsex1 homosex1 abdefct1 abpoor1 abdefctw abpoorw taxcheat govcheat concong conbiz congovt conchurh concourt conschls polsgod clergvte religpub clerggov churhpow theism fatalism godmeans nihilism predeter egomeans ownfate schlpray godright socright perright antirel cantrust trustsci religcon religint religinf carright relgrpeq rspctrel relext1 relext2 reincar nirvana ancestrs paxhappy makefrnd comfort rightpeo obeythnk privent scifaith harmgood sciworse scigrn grnecon harmsgrn anrights resnatur grnprog naturpax grwthelp antests naturwar grwtharm ihlpgrn carsgen nukegen indusgen chemgen watergen tempgen pubdecid busdecid usdoenuf popgrwth impgrn othssame grnexagg genegen amprogrn grnintl ldcgrn econgrn excldimm trust5 fair5 grncon onenatn ambornin amcit amlived amenglsh amchrstn amgovt amfeel belikeus ambetter ifwrong prouddem proudpol proudeco proudsss proudsci proudspt proudart proudmil proudhis proudgrp imports wrldgovt forlang amownway forland amtv amcult mincult meltpot1 immcrime immameco immjobs immideas refugees nafta2 amancstr lessprd intlincs freetrde decsorgs powrorgs forcult internet immimp immcosts kidshere kidsaway immrghts nafta3 shortcom immcult immeduc immassim patriot1 patriot2 patriot3 patriot4 voteelec paytaxes obeylaws watchgov actassoc othreasn buypol helpusa helpwrld milserve relmeet revmeet racmeet solok rghtsmin eqtreat citviews polopts oppsegov govdook polgreed govngos unrghts polactve choices refrndms elecvote elecfair servepeo fixmistk corruptn demtoday demrghts gvtrghts crimlose ntcitvte notvote hlthall creation scitesty forbdcom forbdrac forbdmar allowcom allowrac allowmar aidssch aidsads aidsinsr aidshlth aidsmar aidssxed aidsids aidsfare genegood parhardr parworse parrght parwhere parfin pargovt partime parwork partaxes rolema rolepa rolegp roleccp roletchr roleclrg inffilms infpubtv infnettv infadstv infmusic sppregnt sphlthkd spheadst sppoorkd spwrkpar sphomekd spdsabkd spdrugs spfoodkd sppill sectech secdocs askfinan askcrime askdrugs askmentl askforgn askdrink asksexor askfrbiz askfrtrv askcomp secprvcy secdiplo secmilop secterr secbudgt chkfinan chkspfin chktaxes takearms leakinfo spyenemy spyfrend taketrck punarms punleak punenmy punfrnd puntrck comsteal comdata comsys comsnoop comemail comporn lietest testdrug bugging finanqs chkonjob chkother compfin chktravl emailwrk emailhme tapwrk taphme srchwrk camwrk usintl usun commun russia japan england canada brazil china israel egypt welfare1 welfare2 welfare3 welfare4 welfare5 welfare6 popespks polhitok marsame race age relig sex educ realinc size region prestg10


save GSS_Recoded2024_0204_withdemo.dta, replace
