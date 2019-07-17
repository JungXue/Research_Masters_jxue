
* ------------------------------ Premeables ----------------------------------------------;

options nofmterr linesize=100 pagesize=55 YEARCUTOFF=1900 nonumber nodate formchar = '|_';
options mcompilenote=all mprint  mlogic;

%let dir = H:\Desktop\7.1 No time to waste\Stats 798A Research Master\R-codes\MIMIC;
%let source = &dir\source;
%let temp = &dir\temp;

%include "&source\Macros701.txt";	
%include "&source\MacrosXue.txt";

LIBNAME MIMIC "&dir";
LIBNAME temp "&temp";

title;
footnote;
DM 'odsresults; clear';

* ------------------------------ Import datasets ----------------------------------------------;

PROC IMPORT OUT= temp.ADMISSIONS 
            DATAFILE= "&dir\data\ADMISSIONS.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= temp.DIAGNOSES_ICD
            DATAFILE= "&dir\data\DIAGNOSES_ICD.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= temp.D_ICD_DIAGNOSES
            DATAFILE= "&dir\data\D_ICD_DIAGNOSES.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

* ---------------------------- Data cleanning ------------------------------------------------;

*ADMISSIONS;

data temp.ADMISSIONS_2; set temp.ADMISSIONS ;
if DIAGNOSIS = "" then DELETE;                   /*Delete empty diag values*/
keep SUBJECT_ID HADM_ID 
	 ADMITTIME ADMISSION_TYPE ADMISSION_LOCATION 
	 DIAGNOSIS;
run;

%printX(temp.ADMISSIONS_2,20)
%contentsX(temp.ADMISSIONS_2)
%missingXcn(temp.ADMISSIONS_2);

*DIAGNOSES_ICD;

data temp.DIAGNOSES_ICD_2; set temp.DIAGNOSES_ICD ;
If SEQ_NUM ^= 1 then DELETE;                     /*Keep primary diagnosis*/
keep SUBJECT_ID HADM_ID SEQ_NUM ICD9_CODE ;    
run;

%printX(temp.DIAGNOSES_ICD_2,20)
%contentsX(temp.DIAGNOSES_ICD_2)
%missingXcn(temp.DIAGNOSES_ICD_2);

*D_ICD_DIAGNOSES;

data temp.D_ICD_DIAGNOSES_2; set temp.D_ICD_DIAGNOSES ;
keep ICD9_CODE SHORT_TITLE LONG_TITLE ;
run;

%printX(temp.D_ICD_DIAGNOSES_2,20)
%contentsX(temp.D_ICD_DIAGNOSES_2)
%missingXc(temp.D_ICD_DIAGNOSES_2);

* --------------------------- Data merging -------------------------------------------------;

proc sql;
  create table temp.merge1 as
  select * 
  from temp.DIAGNOSES_ICD_2, temp.D_ICD_DIAGNOSES_2
  where ICD9_CODE = ICD9_CODE
  order by ICD9_CODE;
quit;
%printX(temp.merge1,100)
%contentsX(temp.merge1);
%missingXc(temp.merge1);

data temp.merge2 ; set temp.merge1 ;
If HADM_ID = "" then DELETE;                                          /*delete diagnosis not seen*/
keep SUBJECT_ID HADM_ID SEQ_NUM ICD9_CODE SHORT_TITLE LONG_TITLE  ;    
run;
%printX(temp.merge2,100);
%contentsX(temp.merge2);
%missingXc(temp.merge2);   
 
* Merge each entry by hospital event id ;

%sortX(temp.ADMISSIONS_2,HADM_ID)  
%sortX(temp.merge2,HADM_ID)
data temp.merge3; 
	merge temp.merge2 temp.ADMISSIONS_2;
	by HADM_ID;
If ICD9_CODE = "" then DELETE;
If substr(ICD9_CODE,1,1) in ('0','1','2','3','4','5','6','7','8','9') /* Delete all ICD start with V or E (supplimantry info)*/
then ICD9_CODE = ICD9_CODE ; else DELETE;                             /* This mostly deletes birth events */
ICD9_CODE_FULL = ICD9_CODE;             /* Keep full ICD9_CODE as record */
ICD9_CODE = substr(ICD9_CODE_full,1,3); /* change ICD9_CODE to numeric */
ICD9_CODE2 = input(ICD9_CODE, 7.);
   drop ICD9_CODE;
   rename ICD9_CODE2 = ICD9_CODE;
run;
%printX(temp.merge3,20)
%contentsX(temp.merge3);
%missingXcn(temp.merge3); /*some with missing info*/


* ------------------------- merged data cleanning ---------------------------------------------------;
* Classification criteria based on inofrmation from https://icd.codes/icd9cm ;

data temp.merge3b; set temp.merge3 ;
if ICD9_CODE in(001:139) then ICD_Chapter1 = "001-139" ;else 
if ICD9_CODE in(140:239) then ICD_Chapter1 = "140-239" ;else 
if ICD9_CODE in(240:279) then ICD_Chapter1 = "240-279" ;else 
if ICD9_CODE in(280:289) then ICD_Chapter1 = "280-289" ;else 
if ICD9_CODE in(290:319) then ICD_Chapter1 = "290-319" ;else 

if ICD9_CODE in(320:389) then ICD_Chapter1 = "320-389" ;else 
if ICD9_CODE in(390:459) then ICD_Chapter1 = "390-459" ;else 
if ICD9_CODE in(460:519) then ICD_Chapter1 = "460-519" ;else 
if ICD9_CODE in(520:579) then ICD_Chapter1 = "520-579" ;else 
if ICD9_CODE in(580:629) then ICD_Chapter1 = "580-629" ;else 

if ICD9_CODE in(630:679) then ICD_Chapter1 = "630-679" ;else 
if ICD9_CODE in(680:709) then ICD_Chapter1 = "680-709" ;else 
if ICD9_CODE in(710:739) then ICD_Chapter1 = "710-739" ;else 
if ICD9_CODE in(740:759) then ICD_Chapter1 = "740-759" ;else 
if ICD9_CODE in(760:779) then ICD_Chapter1 = "760-779" ;else 

if ICD9_CODE in(780:799) then ICD_Chapter1 = "780-799" ;else 
if ICD9_CODE in(800:999) then ICD_Chapter1 = "800-999" ;else ICD_Chapter1 = "NA";
run;

%printX(temp.merge3b,20)

data temp.mimic4; set temp.merge3b ;
if ICD9_CODE in(001:009) then ICD_Chapter2 = "001-009" ;*Chapter 1 ;else 
if ICD9_CODE in(010:018) then ICD_Chapter2 = "010-018" ;else
if ICD9_CODE in(020:027) then ICD_Chapter2 = "020-027" ;else
if ICD9_CODE in(030:041) then ICD_Chapter2 = "030-041" ;else
if ICD9_CODE in(042:042) then ICD_Chapter2 = "042-042" ;else

if ICD9_CODE in(045:049) then ICD_Chapter2 = "045-049" ;else
if ICD9_CODE in(050:059) then ICD_Chapter2 = "050-059" ;else
if ICD9_CODE in(060:066) then ICD_Chapter2 = "060-066" ;else
if ICD9_CODE in(070:079) then ICD_Chapter2 = "070-079" ;else
if ICD9_CODE in(080:088) then ICD_Chapter2 = "080-088" ;else 

if ICD9_CODE in(090:099) then ICD_Chapter2 = "090-099" ;else 
if ICD9_CODE in(100:104) then ICD_Chapter2 = "100-104" ;else 
if ICD9_CODE in(110:118) then ICD_Chapter2 = "110-118" ;else 
if ICD9_CODE in(120:129) then ICD_Chapter2 = "120-129" ;else 
if ICD9_CODE in(130:136) then ICD_Chapter2 = "130-136" ;else 

if ICD9_CODE in(137:139) then ICD_Chapter2 = "137-139" ;else 

if ICD9_CODE in(140:149) then ICD_Chapter2 = "140-149" ;*Chapter 2 ;else 
if ICD9_CODE in(150:159) then ICD_Chapter2 = "150-159" ;else 
if ICD9_CODE in(160:165) then ICD_Chapter2 = "160-165" ;else 
if ICD9_CODE in(170:176) then ICD_Chapter2 = "170-176" ;else 
if ICD9_CODE in(179:189) then ICD_Chapter2 = "179-189" ;else 

if ICD9_CODE in(190:199) then ICD_Chapter2 = "190-199" ;else 
if ICD9_CODE in(200:209) then ICD_Chapter2 = "200-209" ;else 
if ICD9_CODE in(210:229) then ICD_Chapter2 = "210-229" ;else 
if ICD9_CODE in(230:234) then ICD_Chapter2 = "230-234" ;else 
if ICD9_CODE in(235:238) then ICD_Chapter2 = "235-238" ;else 

if ICD9_CODE in(239:239) then ICD_Chapter2 = "239-239" ;else 

if ICD9_CODE in(240:246) then ICD_Chapter2 = "240-246" ;*Chapter 3 ;else 
if ICD9_CODE in(249:259) then ICD_Chapter2 = "249-259" ;else 
if ICD9_CODE in(260:269) then ICD_Chapter2 = "260-269" ;else 
if ICD9_CODE in(270:279) then ICD_Chapter2 = "270-279" ;else 

if ICD9_CODE in(280) then ICD_Chapter2 = "280-280" ;*Chapter 4 ;else 
if ICD9_CODE in(281) then ICD_Chapter2 = "281-281" ;else 
if ICD9_CODE in(282) then ICD_Chapter2 = "282-282" ;else 
if ICD9_CODE in(283) then ICD_Chapter2 = "283-283" ;else 
if ICD9_CODE in(284) then ICD_Chapter2 = "284-284" ;else 

if ICD9_CODE in(285) then ICD_Chapter2 = "285-285" ;else 
if ICD9_CODE in(286) then ICD_Chapter2 = "286-286" ;else 
if ICD9_CODE in(287) then ICD_Chapter2 = "287-287" ;else 
if ICD9_CODE in(288) then ICD_Chapter2 = "288-288" ;else 
if ICD9_CODE in(289) then ICD_Chapter2 = "289-289" ;else 

if ICD9_CODE in(290:294) then ICD_Chapter2 = "290-294" ;*Chapter 5 ;else 
if ICD9_CODE in(295:299) then ICD_Chapter2 = "295-299" ;else 
if ICD9_CODE in(300:316) then ICD_Chapter2 = "300-316" ;else 
if ICD9_CODE in(317:319) then ICD_Chapter2 = "317-319" ;else 

if ICD9_CODE in(320:327) then ICD_Chapter2 = "320-327" ;*Chapter 6 ;else 
if ICD9_CODE in(330:337) then ICD_Chapter2 = "330-337" ;else 
if ICD9_CODE in(338:338) then ICD_Chapter2 = "338-338" ;else 
if ICD9_CODE in(339:339) then ICD_Chapter2 = "339-339" ;else 
if ICD9_CODE in(340:349) then ICD_Chapter2 = "340-349" ;else 

if ICD9_CODE in(350:359) then ICD_Chapter2 = "350-359" ;else 
if ICD9_CODE in(360:379) then ICD_Chapter2 = "360-379" ;else 
if ICD9_CODE in(380:389) then ICD_Chapter2 = "380-389" ;else 

if ICD9_CODE in(390:392) then ICD_Chapter2 = "390-392" ;*Chapter 7 ;else 
if ICD9_CODE in(393:398) then ICD_Chapter2 = "393-398" ;else 
if ICD9_CODE in(401:405) then ICD_Chapter2 = "401-405" ;else 
if ICD9_CODE in(410:414) then ICD_Chapter2 = "410-414" ;else 
if ICD9_CODE in(415:417) then ICD_Chapter2 = "415-417" ;else 

if ICD9_CODE in(420:429) then ICD_Chapter2 = "420-429" ;else 
if ICD9_CODE in(430:438) then ICD_Chapter2 = "430-438" ;else 
if ICD9_CODE in(440:449) then ICD_Chapter2 = "440-449" ;else 
if ICD9_CODE in(451:459) then ICD_Chapter2 = "451-459" ;else 

if ICD9_CODE in(460:466) then ICD_Chapter2 = "460-466" ;*Chapter 8 ;else 
if ICD9_CODE in(470:478) then ICD_Chapter2 = "470-478" ;else 
if ICD9_CODE in(480:488) then ICD_Chapter2 = "480-488" ;else 
if ICD9_CODE in(490:496) then ICD_Chapter2 = "490-496" ;else 
if ICD9_CODE in(500:508) then ICD_Chapter2 = "500-508" ;else 

if ICD9_CODE in(510:519) then ICD_Chapter2 = "510-519" ;else 

if ICD9_CODE in(520:529) then ICD_Chapter2 = "520-529" ;*Chapter 9 ;else 
if ICD9_CODE in(530:539) then ICD_Chapter2 = "530-539" ;else 
if ICD9_CODE in(540:543) then ICD_Chapter2 = "540-543" ;else 
if ICD9_CODE in(550:553) then ICD_Chapter2 = "550-553" ;else 
if ICD9_CODE in(555:558) then ICD_Chapter2 = "555-558" ;else 

if ICD9_CODE in(560:569) then ICD_Chapter2 = "560-569" ;else 
if ICD9_CODE in(570:579) then ICD_Chapter2 = "570-579" ;else 

if ICD9_CODE in(580:589) then ICD_Chapter2 = "580-589" ;*Chapter 10 ;else 
if ICD9_CODE in(590:599) then ICD_Chapter2 = "590-599" ;else 
if ICD9_CODE in(600:608) then ICD_Chapter2 = "600-608" ;else 
if ICD9_CODE in(610:612) then ICD_Chapter2 = "610-612" ;else 
if ICD9_CODE in(614:616) then ICD_Chapter2 = "614-616" ;else 

if ICD9_CODE in(617:629) then ICD_Chapter2 = "617-629" ;else 

if ICD9_CODE in(630:639) then ICD_Chapter2 = "630-639" ;*Chapter 11 ;else 
if ICD9_CODE in(640:649) then ICD_Chapter2 = "640-649" ;else 
if ICD9_CODE in(650:659) then ICD_Chapter2 = "650-659" ;else 
if ICD9_CODE in(660:669) then ICD_Chapter2 = "660-669" ;else 
if ICD9_CODE in(670:677) then ICD_Chapter2 = "670-677" ;else 

if ICD9_CODE in(678:679) then ICD_Chapter2 = "678-679" ;else 

if ICD9_CODE in(680:686) then ICD_Chapter2 = "680-686" ;*Chapter 12 ;else 
if ICD9_CODE in(690:698) then ICD_Chapter2 = "690-698" ;else 
if ICD9_CODE in(700:709) then ICD_Chapter2 = "700-709" ;else 

if ICD9_CODE in(710:719) then ICD_Chapter2 = "710-719" ;*Chapter 13 ;else 
if ICD9_CODE in(720:724) then ICD_Chapter2 = "720-724" ;else 
if ICD9_CODE in(725:729) then ICD_Chapter2 = "725-729" ;else 
if ICD9_CODE in(730:739) then ICD_Chapter2 = "730-739" ;else 

if ICD9_CODE in(740) then ICD_Chapter2 = "740-740" ;*Chapter 14 ;else 
if ICD9_CODE in(741) then ICD_Chapter2 = "741-741" ;else 
if ICD9_CODE in(742) then ICD_Chapter2 = "742-742" ;else 
if ICD9_CODE in(743) then ICD_Chapter2 = "743-743" ;else 
if ICD9_CODE in(744) then ICD_Chapter2 = "744-744" ;else 

if ICD9_CODE in(745) then ICD_Chapter2 = "745-745" ;else 
if ICD9_CODE in(746) then ICD_Chapter2 = "746-746" ;else 
if ICD9_CODE in(747) then ICD_Chapter2 = "747-747" ;else 
if ICD9_CODE in(748) then ICD_Chapter2 = "748-748" ;else 
if ICD9_CODE in(749) then ICD_Chapter2 = "749-749" ;else 

if ICD9_CODE in(750) then ICD_Chapter2 = "750-750" ;else 
if ICD9_CODE in(751) then ICD_Chapter2 = "751-751" ;else 
if ICD9_CODE in(752) then ICD_Chapter2 = "752-752" ;else 
if ICD9_CODE in(753) then ICD_Chapter2 = "753-753" ;else 
if ICD9_CODE in(754) then ICD_Chapter2 = "754-754" ;else 

if ICD9_CODE in(755) then ICD_Chapter2 = "755-755" ;else 
if ICD9_CODE in(756) then ICD_Chapter2 = "756-756" ;else 
if ICD9_CODE in(757) then ICD_Chapter2 = "757-757" ;else 
if ICD9_CODE in(758) then ICD_Chapter2 = "758-758" ;else 
if ICD9_CODE in(759) then ICD_Chapter2 = "759-759" ;else 

if ICD9_CODE in(760:763) then ICD_Chapter2 = "760-763" ;*Chapter 15 ;else 
if ICD9_CODE in(764:779) then ICD_Chapter2 = "764-779" ;else 

if ICD9_CODE in(780:789) then ICD_Chapter2 = "780-789" ;*Chapter 16 ;else 
if ICD9_CODE in(790:796) then ICD_Chapter2 = "790-796" ;else 
if ICD9_CODE in(797:799) then ICD_Chapter2 = "797-799" ;else 

if ICD9_CODE in(800:804) then ICD_Chapter2 = "800-804" ;*Chapter 17 ;else 
if ICD9_CODE in(805:809) then ICD_Chapter2 = "805-809" ;else 
if ICD9_CODE in(810:819) then ICD_Chapter2 = "810-819" ;else 
if ICD9_CODE in(820:829) then ICD_Chapter2 = "820-829" ;else 
if ICD9_CODE in(830:839) then ICD_Chapter2 = "830-839" ;else 

if ICD9_CODE in(840:848) then ICD_Chapter2 = "840-848" ;else 
if ICD9_CODE in(850:854) then ICD_Chapter2 = "850-854" ;else 
if ICD9_CODE in(860:869) then ICD_Chapter2 = "860-869" ;else 
if ICD9_CODE in(870:879) then ICD_Chapter2 = "870-879" ;else 
if ICD9_CODE in(880:887) then ICD_Chapter2 = "880-887" ;else 

if ICD9_CODE in(890:897) then ICD_Chapter2 = "890-897" ;else 
if ICD9_CODE in(900:904) then ICD_Chapter2 = "900-904" ;else 
if ICD9_CODE in(905:909) then ICD_Chapter2 = "905-909" ;else 
if ICD9_CODE in(910:919) then ICD_Chapter2 = "910-919" ;else 
if ICD9_CODE in(920:924) then ICD_Chapter2 = "920-924" ;else 

if ICD9_CODE in(925:929) then ICD_Chapter2 = "925-929" ;else 
if ICD9_CODE in(930:939) then ICD_Chapter2 = "930-939" ;else 
if ICD9_CODE in(940:949) then ICD_Chapter2 = "940-949" ;else 
if ICD9_CODE in(950:957) then ICD_Chapter2 = "950-957" ;else 
if ICD9_CODE in(958:959) then ICD_Chapter2 = "958-959" ;else 

if ICD9_CODE in(960:979) then ICD_Chapter2 = "960-979" ;else 
if ICD9_CODE in(980:989) then ICD_Chapter2 = "980-989" ;else 
if ICD9_CODE in(990:995) then ICD_Chapter2 = "990-995" ;else 
if ICD9_CODE in(996:999) then ICD_Chapter2 = "996-999" ;else ICD_Chapter2 = "NA";

run;

data temp.mimic4b; set temp.mimic4 ;
length ICD_Chapter1_Des $100 ; 
if ICD9_CODE in(001:139) then ICD_Chapter1_Des = "Infectious And Parasitic Diseases" ;else 
if ICD9_CODE in(140:239) then ICD_Chapter1_Des = "Neoplasms" ;else 
if ICD9_CODE in(240:279) then ICD_Chapter1_Des = "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders" ;else 
if ICD9_CODE in(280:289) then ICD_Chapter1_Des = "Diseases Of The Blood And Blood-Forming Organs" ;else
if ICD9_CODE in(290:319) then ICD_Chapter1_Des = "Mental Disorders" ;else 

if ICD9_CODE in(320:389) then ICD_Chapter1_Des = "Diseases Of The Nervous System And Sense Organs" ;else 
if ICD9_CODE in(390:459) then ICD_Chapter1_Des = "Diseases Of The Circulatory System" ;else 
if ICD9_CODE in(460:519) then ICD_Chapter1_Des = "Diseases Of The Respiratory System" ;else
if ICD9_CODE in(520:579) then ICD_Chapter1_Des = "Diseases Of The Digestive System" ;else 
if ICD9_CODE in(580:629) then ICD_Chapter1_Des = "Diseases Of The Genitourinary System" ;else 

if ICD9_CODE in(630:679) then ICD_Chapter1_Des = "Complications Of Pregnancy, Childbirth, And The Puerperium" ;else 
if ICD9_CODE in(680:709) then ICD_Chapter1_Des = "Diseases Of The Skin And Subcutaneous Tissue" ;else
if ICD9_CODE in(710:739) then ICD_Chapter1_Des = "Diseases Of The Musculoskeletal System And Connective Tissue" ;else 
if ICD9_CODE in(740:759) then ICD_Chapter1_Des = "Congenital Anomalies" ;else 
if ICD9_CODE in(760:779) then ICD_Chapter1_Des = "Certain Conditions Originating In The Perinatal Period" ;else 

if ICD9_CODE in(780:799) then ICD_Chapter1_Des = "Symptoms, Signs, And Ill-Defined Conditions" ;else 
if ICD9_CODE in(800:999) then ICD_Chapter1_Des = "Injury And Poisoning" ;else ICD_Chapter1_Des = "NA";
run;

data temp.mimic5; set temp.mimic4b ;

length ICD_Chapter2_Des $100 ; 

if ICD9_CODE in(001:009) then ICD_Chapter2_Des = "Intestinal Infectious Diseases" ;*Chapter 1 ;else 
if ICD9_CODE in(010:018) then ICD_Chapter2_Des = "Tuberculosis" ;else
if ICD9_CODE in(020:027) then ICD_Chapter2_Des = "Zoonotic Bacterial Diseases" ;else
if ICD9_CODE in(030:041) then ICD_Chapter2_Des = "Other Bacterial Diseases" ;else
if ICD9_CODE in(042:042) then ICD_Chapter2_Des = "Human Immunodeficiency Virus" ;else

if ICD9_CODE in(045:049) then ICD_Chapter2_Des = "Poliomyelitis And Other Non-Arthropod-Borne Viral Diseases Of Central Nervous System" ;else
if ICD9_CODE in(050:059) then ICD_Chapter2_Des = "Viral Diseases Accompanied By Exanthem" ;else
if ICD9_CODE in(060:066) then ICD_Chapter2_Des = "Arthropod-Borne Viral Diseases" ;else
if ICD9_CODE in(070:079) then ICD_Chapter2_Des = "Other Diseases Due To Viruses And Chlamydiae" ;else
if ICD9_CODE in(080:088) then ICD_Chapter2_Des = "Rickettsioses And Other Arthropod-Borne Diseases" ;else 

if ICD9_CODE in(090:099) then ICD_Chapter2_Des = "Syphilis And Other Venereal Diseases" ;else 
if ICD9_CODE in(100:104) then ICD_Chapter2_Des = "Other Spirochetal Diseases" ;else 
if ICD9_CODE in(110:118) then ICD_Chapter2_Des = "Mycoses" ;else 
if ICD9_CODE in(120:129) then ICD_Chapter2_Des = "Helminthiases" ;else 
if ICD9_CODE in(130:136) then ICD_Chapter2_Des = "Other Infectious And Parasitic Diseases" ;else 

if ICD9_CODE in(137:139) then ICD_Chapter2_Des = "Late Effects Of Infectious And Parasitic Diseases" ;else 

if ICD9_CODE in(140:149) then ICD_Chapter2_Des = "Malignant Neoplasm Of Lip, Oral Cavity, And Pharynx" ;*Chapter 2 ;else 
if ICD9_CODE in(150:159) then ICD_Chapter2_Des = "Malignant Neoplasm Of Digestive Organs And Peritoneum" ;else 
if ICD9_CODE in(160:165) then ICD_Chapter2_Des = "Malignant Neoplasm Of Respiratory And Intrathoracic Organs" ;else 
if ICD9_CODE in(170:176) then ICD_Chapter2_Des = "Malignant Neoplasm Of Bone, Connective Tissue, Skin, And Breast" ;else 
if ICD9_CODE in(179:189) then ICD_Chapter2_Des = "Malignant Neoplasm Of Genitourinary Organs" ;else 

if ICD9_CODE in(190:199) then ICD_Chapter2_Des = "Malignant Neoplasm Of Other And Unspecified Sites" ;else 
if ICD9_CODE in(200:209) then ICD_Chapter2_Des = "Malignant Neoplasm Of Lymphatic And Hematopoietic Tissue" ;else 
if ICD9_CODE in(210:229) then ICD_Chapter2_Des = "Benign Neoplasms" ;else 
if ICD9_CODE in(230:234) then ICD_Chapter2_Des = "Carcinoma In Situ" ;else 
if ICD9_CODE in(235:238) then ICD_Chapter2_Des = "Neoplasms Of Uncertain Behavior" ;else 

if ICD9_CODE in(239:239) then ICD_Chapter2_Des = "Neoplasms Of Unspecified Nature" ;else 

if ICD9_CODE in(240:246) then ICD_Chapter2_Des = "Disorders Of Thyroid Gland" ;*Chapter 3 ;else 
if ICD9_CODE in(249:259) then ICD_Chapter2_Des = "Diseases Of Other Endocrine Glands" ;else 
if ICD9_CODE in(260:269) then ICD_Chapter2_Des = "Nutritional Deficiencies" ;else 
if ICD9_CODE in(270:279) then ICD_Chapter2_Des = "Other Metabolic Disorders And Immunity Disorders" ;else 

if ICD9_CODE in(280) then ICD_Chapter2_Des = "Iron deficiency anemias" ;*Chapter 4 ;else 
if ICD9_CODE in(281) then ICD_Chapter2_Des = "Other deficiency anemias" ;else 
if ICD9_CODE in(282) then ICD_Chapter2_Des = "Hereditary hemolytic anemias" ;else 
if ICD9_CODE in(283) then ICD_Chapter2_Des = "Acquired hemolytic anemias" ;else 
if ICD9_CODE in(284) then ICD_Chapter2_Des = "Aplastic anemia and other bone marrow failure syndromes" ;else 

if ICD9_CODE in(285) then ICD_Chapter2_Des = "Other and unspecified anemias" ;else 
if ICD9_CODE in(286) then ICD_Chapter2_Des = "Coagulation defects" ;else 
if ICD9_CODE in(287) then ICD_Chapter2_Des = "Purpura and other hemorrhagic conditions" ;else 
if ICD9_CODE in(288) then ICD_Chapter2_Des = "Diseases of white blood cells" ;else 
if ICD9_CODE in(289) then ICD_Chapter2_Des = "Other diseases of blood and blood-forming organs" ;else 

if ICD9_CODE in(290:294) then ICD_Chapter2_Des = "Organic Psychotic Conditions" ;*Chapter 5 ;else 
if ICD9_CODE in(295:299) then ICD_Chapter2_Des = "Other Psychoses" ;else 
if ICD9_CODE in(300:316) then ICD_Chapter2_Des = "Neurotic Disorders, Personality Disorders, And Other Nonpsychotic Mental Disorders" ;else 
if ICD9_CODE in(317:319) then ICD_Chapter2_Des = "Intellectual Disabilities" ;else 

if ICD9_CODE in(320:327) then ICD_Chapter2_Des = "Inflammatory Diseases Of The Central Nervous System" ;*Chapter 6 ;else 
if ICD9_CODE in(330:337) then ICD_Chapter2_Des = "Hereditary And Degenerative Diseases Of The Central Nervous System" ;else 
if ICD9_CODE in(338:338) then ICD_Chapter2_Des = "Pain" ;else 
if ICD9_CODE in(339:339) then ICD_Chapter2_Des = "Other Headache Syndromes" ;else 
if ICD9_CODE in(340:349) then ICD_Chapter2_Des = "Other Disorders Of The Central Nervous System" ;else 

if ICD9_CODE in(350:359) then ICD_Chapter2_Des = "Disorders Of The Peripheral Nervous System" ;else 
if ICD9_CODE in(360:379) then ICD_Chapter2_Des = "Disorders Of The Eye And Adnexa" ;else 
if ICD9_CODE in(380:389) then ICD_Chapter2_Des = "Diseases Of The Ear And Mastoid Process" ;else 

if ICD9_CODE in(390:392) then ICD_Chapter2_Des = "Acute Rheumatic Fever" ;*Chapter 7 ;else 
if ICD9_CODE in(393:398) then ICD_Chapter2_Des = "Chronic Rheumatic Heart Disease" ;else 
if ICD9_CODE in(401:405) then ICD_Chapter2_Des = "Hypertensive Disease" ;else 
if ICD9_CODE in(410:414) then ICD_Chapter2_Des = "Ischemic Heart Disease" ;else 
if ICD9_CODE in(415:417) then ICD_Chapter2_Des = "Diseases Of Pulmonary Circulation" ;else 

if ICD9_CODE in(420:429) then ICD_Chapter2_Des = "Other Forms Of Heart Disease" ;else 
if ICD9_CODE in(430:438) then ICD_Chapter2_Des = "Cerebrovascular Disease" ;else 
if ICD9_CODE in(440:449) then ICD_Chapter2_Des = "Diseases Of Arteries, Arterioles, And Capillaries" ;else 
if ICD9_CODE in(451:459) then ICD_Chapter2_Des = "Diseases Of Veins And Lymphatics, And Other Diseases Of Circulatory System" ;else 

if ICD9_CODE in(460:466) then ICD_Chapter2_Des = "Acute Respiratory Infections" ;*Chapter 8 ;else 
if ICD9_CODE in(470:478) then ICD_Chapter2_Des = "Other Diseases Of Upper Respiratory Tract" ;else 
if ICD9_CODE in(480:488) then ICD_Chapter2_Des = "Pneumonia And Influenza" ;else 
if ICD9_CODE in(490:496) then ICD_Chapter2_Des = "Chronic Obstructive Pulmonary Disease And Allied Conditions" ;else 
if ICD9_CODE in(500:508) then ICD_Chapter2_Des = "Pneumoconioses And Other Lung Diseases Due To External Agents" ;else 

if ICD9_CODE in(510:519) then ICD_Chapter2_Des = "Other Diseases Of Respiratory System" ;else 

if ICD9_CODE in(520:529) then ICD_Chapter2_Des = "Diseases Of Oral Cavity, Salivary Glands, And Jaws" ;*Chapter 9 ;else 
if ICD9_CODE in(530:539) then ICD_Chapter2_Des = "Diseases Of Esophagus, Stomach, And Duodenum" ;else 
if ICD9_CODE in(540:543) then ICD_Chapter2_Des = "Appendicitis" ;else 
if ICD9_CODE in(550:553) then ICD_Chapter2_Des = "Hernia Of Abdominal Cavity" ;else 
if ICD9_CODE in(555:558) then ICD_Chapter2_Des = "Noninfective Enteritis And Colitis" ;else 

if ICD9_CODE in(560:569) then ICD_Chapter2_Des = "Other Diseases Of Intestines And Peritoneum" ;else 
if ICD9_CODE in(570:579) then ICD_Chapter2_Des = "Other Diseases Of Digestive System" ;else 

if ICD9_CODE in(580:589) then ICD_Chapter2_Des = "Nephritis, Nephrotic Syndrome, And Nephrosis" ;*Chapter 10 ;else 
if ICD9_CODE in(590:599) then ICD_Chapter2_Des = "Other Diseases Of Urinary System" ;else 
if ICD9_CODE in(600:608) then ICD_Chapter2_Des = "Diseases Of Male Genital Organs" ;else 
if ICD9_CODE in(610:612) then ICD_Chapter2_Des = "Disorders Of Breast" ;else 
if ICD9_CODE in(614:616) then ICD_Chapter2_Des = "Inflammatory Disease Of Female Pelvic Organs" ;else 

if ICD9_CODE in(617:629) then ICD_Chapter2_Des = "Other Disorders Of Female Genital Tract" ;else 

if ICD9_CODE in(630:639) then ICD_Chapter2_Des = "Ectopic And Molar Pregnancy And Other Pregnancy With Abortive Outcome" ;*Chapter 11 ;else 
if ICD9_CODE in(640:649) then ICD_Chapter2_Des = "Complications Mainly Related To Pregnancy" ;else 
if ICD9_CODE in(650:659) then ICD_Chapter2_Des = "Normal Delivery, And Other Indications For Care In Pregnancy, Labor, And Delivery" ;else 
if ICD9_CODE in(660:669) then ICD_Chapter2_Des = "Complications Occurring Mainly In The Course Of Labor And Delivery" ;else 
if ICD9_CODE in(670:677) then ICD_Chapter2_Des = "Complications Of The Puerperium" ;else 

if ICD9_CODE in(678:679) then ICD_Chapter2_Des = "Other Maternal And Fetal Complications" ;else 

if ICD9_CODE in(680:686) then ICD_Chapter2_Des = "Infections Of Skin And Subcutaneous Tissue" ;*Chapter 12 ;else 
if ICD9_CODE in(690:698) then ICD_Chapter2_Des = "Other Inflammatory Conditions Of Skin And Subcutaneous Tissue" ;else 
if ICD9_CODE in(700:709) then ICD_Chapter2_Des = "Other Diseases Of Skin And Subcutaneous Tissue" ;else 

if ICD9_CODE in(710:719) then ICD_Chapter2_Des = "Arthropathies And Related Disorders" ;*Chapter 13 ;else 
if ICD9_CODE in(720:724) then ICD_Chapter2_Des = "Dorsopathies" ;else 
if ICD9_CODE in(725:729) then ICD_Chapter2_Des = "Rheumatism, Excluding The Back" ;else 
if ICD9_CODE in(730:739) then ICD_Chapter2_Des = "Osteopathies, Chondropathies, And Acquired Musculoskeletal Deformities" ;else 

if ICD9_CODE in(740) then ICD_Chapter2_Des = "Anencephalus and similar anomalies" ;*Chapter 14 ;else 
if ICD9_CODE in(741) then ICD_Chapter2_Des = "Spina bifida" ;else 
if ICD9_CODE in(742) then ICD_Chapter2_Des = "Other congenital anomalies of nervous system" ;else 
if ICD9_CODE in(743) then ICD_Chapter2_Des = "Congenital anomalies of eye" ;else 
if ICD9_CODE in(744) then ICD_Chapter2_Des = "Congenital anomalies of ear face and neck" ;else 

if ICD9_CODE in(745) then ICD_Chapter2_Des = "Bulbus cordis anomalies and anomalies of cardiac septal closure" ;else 
if ICD9_CODE in(746) then ICD_Chapter2_Des = "Other congenital anomalies of heart" ;else 
if ICD9_CODE in(747) then ICD_Chapter2_Des = "Other congenital anomalies of circulatory system" ;else 
if ICD9_CODE in(748) then ICD_Chapter2_Des = "Congenital anomalies of respiratory system" ;else 
if ICD9_CODE in(749) then ICD_Chapter2_Des = "Cleft palate and cleft lip" ;else 

if ICD9_CODE in(750) then ICD_Chapter2_Des = "Other congenital anomalies of upper alimentary tract" ;else 
if ICD9_CODE in(751) then ICD_Chapter2_Des = "Other congenital anomalies of digestive system" ;else 
if ICD9_CODE in(752) then ICD_Chapter2_Des = "Congenital anomalies of genital organs" ;else 
if ICD9_CODE in(753) then ICD_Chapter2_Des = "Congenital anomalies of urinary system" ;else 
if ICD9_CODE in(754) then ICD_Chapter2_Des = "Certain congenital musculoskeletal deformities" ;else 

if ICD9_CODE in(755) then ICD_Chapter2_Des = "Other congenital anomalies of limbs" ;else 
if ICD9_CODE in(756) then ICD_Chapter2_Des = "Other congenital musculoskeletal anomalies" ;else 
if ICD9_CODE in(757) then ICD_Chapter2_Des = "Congenital anomalies of the integument" ;else 
if ICD9_CODE in(758) then ICD_Chapter2_Des = "Chromosomal anomalies" ;else 
if ICD9_CODE in(759) then ICD_Chapter2_Des = "Other and unspecified congenital anomalies" ;else 

if ICD9_CODE in(760:763) then ICD_Chapter2_Des = "Maternal Causes Of Perinatal Morbidity And Mortality" ;*Chapter 15 ;else 
if ICD9_CODE in(764:779) then ICD_Chapter2_Des = "Other Conditions Originating In The Perinatal Period" ;else 

if ICD9_CODE in(780:789) then ICD_Chapter2_Des = "Symptoms" ;*Chapter 16 ;else 
if ICD9_CODE in(790:796) then ICD_Chapter2_Des = "Nonspecific Abnormal Findings" ;else 
if ICD9_CODE in(797:799) then ICD_Chapter2_Des = "Ill-Defined And Unknown Causes Of Morbidity And Mortality" ;else 

if ICD9_CODE in(800:804) then ICD_Chapter2_Des = "Fracture Of Skull" ;*Chapter 17 ;else 
if ICD9_CODE in(805:809) then ICD_Chapter2_Des = "Fracture Of Spine And Trunk" ;else 
if ICD9_CODE in(810:819) then ICD_Chapter2_Des = "Fracture Of Upper Limb" ;else 
if ICD9_CODE in(820:829) then ICD_Chapter2_Des = "Fracture Of Lower Limb" ;else 
if ICD9_CODE in(830:839) then ICD_Chapter2_Des = "Dislocation" ;else 

if ICD9_CODE in(840:848) then ICD_Chapter2_Des = "Sprains And Strains Of Joints And Adjacent Muscles" ;else 
if ICD9_CODE in(850:854) then ICD_Chapter2_Des = "Intracranial Injury, Excluding Those With Skull Fracture" ;else 
if ICD9_CODE in(860:869) then ICD_Chapter2_Des = "Internal Injury Of Chest, Abdomen, And Pelvis" ;else 
if ICD9_CODE in(870:879) then ICD_Chapter2_Des = "Open Wound Of Head, Neck, And Trunk" ;else 
if ICD9_CODE in(880:887) then ICD_Chapter2_Des = "Open Wound Of Upper Limb" ;else 

if ICD9_CODE in(890:897) then ICD_Chapter2_Des = "Open Wound Of Lower Limb" ;else 
if ICD9_CODE in(900:904) then ICD_Chapter2_Des = "Injury To Blood Vessels" ;else 
if ICD9_CODE in(905:909) then ICD_Chapter2_Des = "Late Effects Of Injuries, Poisonings, Toxic Effects, And Other External Causes" ;else 
if ICD9_CODE in(910:919) then ICD_Chapter2_Des = "Superficial Injury" ;else 
if ICD9_CODE in(920:924) then ICD_Chapter2_Des = "Contusion With Intact Skin Surface" ;else 

if ICD9_CODE in(925:929) then ICD_Chapter2_Des = "Crushing Injury" ;else 
if ICD9_CODE in(930:939) then ICD_Chapter2_Des = "Effects Of Foreign Body Entering Through Orifice" ;else 
if ICD9_CODE in(940:949) then ICD_Chapter2_Des = "Burns" ;else 
if ICD9_CODE in(950:957) then ICD_Chapter2_Des = "Injury To Nerves And Spinal Cord" ;else 
if ICD9_CODE in(958:959) then ICD_Chapter2_Des = "Certain Traumatic Complications And Unspecified Injuries" ;else 

if ICD9_CODE in(960:979) then ICD_Chapter2_Des = "Poisoning By Drugs, Medicinals And Biological Substances" ;else 
if ICD9_CODE in(980:989) then ICD_Chapter2_Des = "Toxic Effects Of Substances Chiefly Nonmedicinal As To Source" ;else 
if ICD9_CODE in(990:995) then ICD_Chapter2_Des = "Other And Unspecified Effects Of External Causes" ;else 
if ICD9_CODE in(996:999) then ICD_Chapter2_Des = "Complications Of Surgical And Medical Care, Not Elsewhere Classified" ;else ICD_Chapter2_Des = "NA";

run;

%printX(temp.mimic5,20)
%contentsX(temp.mimic5);
%missingXcn(temp.mimic5); /*some with missing info*/

*---------------- Final Data cleanning --------------------------------------------------;

data temp.mimic6; set temp.mimic5 ;  
ADMITDATE = Datepart(ADMITTIME);
ICD9_CODE2 = Put(ICD9_CODE, Z3.);
yeartest = year(ADMITDATE);
drop ICD9_CODE;
rename ICD9_CODE_full = ICD9_full ICD9_CODE2 = ICD9_lv3 ICD_Chapter1 = ICD9_lv1 ICD_Chapter1_Des  = ICD9_lv1_D ICD_Chapter2 = ICD9_lv2 ICD_Chapter2_Des  = ICD9_lv2_D;
format ADMITDATE DDMMYYS10.;
run;

%printX(temp.mimic6,20)
%contentsX(temp.mimic6)


*----------some year were read wrongly, shift year over 2200 100years down;

PROC FREQ DATA=temp.mimic6;
    TABLES yeartest;
RUN;

data temp.mimic6b; set temp.mimic6 ; 
if year(ADMITDATE) in(2201:2300) then ADMITDATE = intnx('year', ADMITDATE, -101, 'same');
if year(ADMITDATE)= 2099 then ADMITDATE = intnx('year', ADMITDATE, 1, 'same');
if year(ADMITDATE)="" then DELETE;
yeartest2 = year(ADMITDATE);
run;

PROC FREQ DATA=temp.mimic6b;
    TABLES yeartest2;
RUN;
*----------;

DATA temp.mimic7;
FORMAT  HADM_ID ADMITDATE ICD9_full ICD9_lv1 ICD9_lv2 ICD9_lv3 ICD9_lv1_D ICD9_lv2_D
SUBJECT_ID SEQ_NUM ADMITTIME  ADMISSION_TYPE ADMISSION_LOCATION
DIAGNOSIS SHORT_TITLE LONG_TITLE ICD_CH2_D ;
SET temp.mimic6b;

%printX(temp.mimic7,20)
%contentsX(temp.mimic7)
%missingXcn(temp.mimic7);

*---------------- Export final data for record--------------------------------------------------;

PROC EXPORT DATA= temp.mimic7 
            OUTFILE= "&dir\data\MIMIC3.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


