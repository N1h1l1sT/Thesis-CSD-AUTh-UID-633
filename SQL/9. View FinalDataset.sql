/******************
**** View Erga ****
******************/
USE YLIKA_KOSTOL
GO

ALTER VIEW [dbo].[v9FinalDataset]
--ALTER VIEW [dbo].[v9FinalDataset]
AS
SELECT [Label] -- LABELLED DATA: Whether the Project was done or not
	  ,[ID] AS [ID_Erga] --Unique Identification for each row
	  ,[TimeSeriesDate] -- Which Quarter of which year the project began
	  ,[MONADA] AS [GrafioEktelesisErgou] --Described as: "Grafio ektelesis ergou" and also as "Geografiki perioxi ergou"
	  ,[Onoma_Polis] --The name of the city in which the project takes places
	  ,[GeoLocX] --Column Should be added to the Original Table as INT NULL (with no default value)
	  ,[GeoLocY] --Column Should be added to the Original Table as INT NULL (with no default value)
	  --,[Hmerominia] --╪ПОТЕ УПэЯВЕИ Г гЛЕЯ_лЕКщТГР, ПАъЯМЕИ АУТч ТГМ ТИЛч, АККИЧР ПАъЯМЕИ ТГМ ТИЛч ТГР гЛЕЯ_аМАЦЦЕКъАР - оУСИАСТИЙэ ЕъМАИ ЙАИ ПэКИ Г ГЛЕЯОЛОМъА ЛЕКщТГР, АККэ ЛЕ КИЦЭТЕЯА NULL
      --,[глея_лекетгс] AS [Imer_Meletis] --чЯХЕ О ПЕКэТГР ЛЕ ЙэПОИО АъТГЛА --пАъЯМЕИ ТИЛч ТГМ ГЛЕЯОЛГМъА ПОУ щВЕИ ТЕКЕИЧСЕИ Г ЛЕКщТГ
      --,[глея_амаццекиас] AS [Imer_Anagelias] --сТщКМОУЛЕ ЦЯэЛЛА ПОУ ИСВЩЕИ ТЯЕИР ЛчМЕР ЙАИ ТОУ КщЛЕ ТО ПОСЭ ТГР СУЛЛЕТОВчР ПОУ ПЯщПЕИ МА ПКГЯЧСЕИ ЦИА ТО щЯЦО (СВЕДЭМ ПэМТА, ъДИА ГЛЕЯОЛГМъА ЛЕ гЛЕЯ_лЕКщТГР)
      --,[глея_упоцяажгс] AS [Imer_Ypografis] --LABELED DATA: еъМАИ ПЕДъО ПОУ ПЯщПЕИ МА ПЯОБКщЬОУЛЕ ДГК. АМ ХА ЦъМЕИ Г ПКГЯЫЛЕъ ЙАИ ПЭТЕ. аПЭ ТГ СТИЦЛч ПОУ ТО ПЕДъО ПАъЯМЕИ ТИЛч (date) ВЯЕИАФЭЛАСТЕ ТА УКИЙэ
      --,[ID2] --inconsequential
      --,[ID_пяотасгс] AS [ID_Protasis] --inconsequential
      --,[етос] AS [Etos] --Subsumed under the TimeSeriesDate paradigm --inconsequential
      --,[а_а] AS [A_A] --inconsequential
      --,[глея_йатавыягсгс] AS [Imer_Kataxorisis] --inconsequential
      --,[аяихлос] AS [Arithmos] --inconsequential
      --,[ваяайтгяислос_еяцоу] --inconsequential --Completely Missing
      --,[сйопос_еяцоу] --inconsequential --Completely Missing
      --,[етос_еяцоу] --inconsequential --Completely Missing
      --,[аяихлос_еяцоу] --inconsequential --Completely Missing
      --,[айуяыхем] AS [Akyrothen] --Rows cannot be deleted, hence if the need arises due to an erroneous entry, it's reflected here
      ,[йатгцояиа] AS [Katigoria] --Similar to [SAP_ваяайтгяислос_еяцоу] but more general
	  ,[Xaraktirismos_Ergou] --Main project's category
	  ,[Skopos_Ergou] --Project's Sub-category
      --,[жоп_коипа] AS [FOP_Lipa] --inconsequential
      --,[фглиа_паяакацг] AS [Zimia_Paralagi] --inconsequential
      --,[олада] AS [Omada] --inconsequential
      --,[етос_лекетгс] AS [Etos_Meletis] --inconsequential
      --,[аяихлос_лекетгс] AS [Arithmos_Meletis] --inconsequential
      --,[еидос_енупгяетгсгс0] AS [Eidos_Eksipiretisis0] --inconsequential
      --,[етос_йатасйеугс] AS [Etos_Kataskevis] --inconsequential
      --,[аяихлос_йатасйеугс] AS [Arithmos_Kataskevis] --inconsequential
      --,[дег_пекатгс] AS [DEH_Pelatis] -- 1=DEH, 0,2,NULL = Client --We ONLY care about the Clients, not DEH
      --,[ая_пяытойокоу_пекатг] AS [Ar_Protokolou_Pelati]  --inconsequential --Mostly Missing --inconsequential
      --,[ая_пяытойокоу_дег] AS [Ar_Protokolou_DEH] --inconsequential --Mostly Missing --inconsequential
      --,[мео_паяаккацг] --Mostly Missing --inconsequential
      --,[ая_паяовгс] AS [Ar_Paroxis] --Mostly Missing --inconsequential
      --,[пеяицяажг] --inconsequential
      --,[глея_аитгсгс] AS [Imer_Aitisis] --inconsequential
      --,[аявг_паяатгя_лекетгс] --Mostly Missing --inconsequential
      --,[текос_паяатгя_лекетгс] --Mostly Missing --inconsequential
	  ,[MelClientDelay] --Whether there was a delay in the study caused by the client
	  ,[Mel_Kathisterisi_Pelati] --The delay, in days, caused by the client
      --,[лек_йахустеягсг_пекатг] AS [Mel_Kathisterisi_Pelati] --
      --,[лек_вуй_йахустеягсг_пекатг] AS [Mel_Xik_Kathisterisi_Pelati] --
      --,[лек_емд_йах_пекатг] AS [Mel_End_Kath_Pelati] --аМ ЙэПОИОР НЕЙИМчСЕИ ЛИА ЙАХУСТщЯГСГ ТГМ ГЛЕЯОЛГМъА апо  ЙАИ ДЕМ ЙАТАВЧЯГСГ ТО еыс ТО binary ХА ЦъМЕИ 1 АККэ ДЕМ ХА щВЕИ СЩМОКО ГЛЕЯЧМ ЙАХУСТщЯГСГР. 
	  ,[MelDEHDelay]
	  ,[Mel_Kathisterisi_DEH] --The delay, in days, caused by the DEDDHE
      --,[лек_йахустеягсг_дег] AS [Mel_Kathisterisi_DEH] --Should facilitate feature engineer with "XYK" and "END"
      --,[лек_вуй_йахустеягсг_дег] AS [Mel_Xyk_Kathisterisi_DEH] --Should facilitate feature engineer with "XYK" and "END"
      --,[лек_емд_йах_дег] AS [Mel_End_Kath_DEH] --аМ ЙэПОИОР НЕЙИМчСЕИ ЛИА ЙАХУСТщЯГСГ ТГМ ГЛЕЯОЛГМъА апо  ЙАИ ДЕМ ЙАТАВЧЯГСГ ТО еыс ТО binary ХА ЦъМЕИ 1 АККэ ДЕМ ХА щВЕИ СЩМОКО ГЛЕЯЧМ ЙАХУСТщЯГСГР. 
      ,[MelOthersDelay]
	  ,[Mel_Kathisterisi_Triton]
	  --,[лек_йахустеягсг_тяитым] AS [Mel_Kathisterisi_Triton] --Should facilitate feature engineer with "XYK" and "END"
      --,[лек_вуй_йахустеягсг_тяитым] AS [Mel_Xyk_Kathisterisi_Triton] --Should facilitate feature engineer with "XYK" and "END"
      --,[лек_емд_йах_тяитым] AS [Mel_End_Kath_Triton] --аМ ЙэПОИОР НЕЙИМчСЕИ ЛИА ЙАХУСТщЯГСГ ТГМ ГЛЕЯОЛГМъА апо  ЙАИ ДЕМ ЙАТАВЧЯГСГ ТО еыс ТО binary ХА ЦъМЕИ 1 АККэ ДЕМ ХА щВЕИ СЩМОКО ГЛЕЯЧМ ЙАХУСТщЯГСГР. 
      ,[глеяес_лекетгс] AS [Meres_Meletis] --The number of days that the 'Study' part of the project lasted, including non-DEDEH delays
      --,[еяц_глеяес_лекетгс] AS [Erg_Meres_Meletis] --We already have the Days it took
      ,UPPER(LTRIM(RTRIM([сумеяцеио_лекетгс]))) AS [Sinergio_Meletis] --To sinergio, 3 Factor Levels
      --,UPPER(LTRIM(RTRIM([лекетгтгс]))) AS [Meletitis] --1932 Meletites
      --,[аы_йатасйеугс]
      --,[йостос_лекетгтг] AS [Kostos_Meletiti] --Mostly Missing
      ,[йостос_еяцатийым_йатасйеугс] AS [Kostos_Ergatikon_Kataskevis]
      ,[йостос_укийым_йатасйеугс] AS [Kostos_Ilikon_Kataskevis]
      ,[йостос_йатасйеугс] AS [Kostos_Kataskevis]
      ,[йостос_еяцокабийым_епидосгс] AS [Kostos_Ergolavikon_Epidosis]
      --,[аы_лекетгс] --inconsequential
      --,[глея_паяакабгс] --inconsequential --пЭТЕ ПчЦЕ СТОМ ЕЯЦОКэБО
      --,[еидос_енупгяетгсгс] AS [Idos_Eksipiretisis] --inconsequential
      --,[титкос_еяцоу] --inconsequential
      --,[сулб_глея_емаянгс] AS [Symb_Imer_Enarksis] --inconsequential --СУЛБАТИЙч ГЛЕЯОЛГМъА. еСЫТЕЯИЙЭ ПЕДъО. уПэЯВЕИ щМА АМЧТЕЯО ЭЯИО МА ТО ЖТИэНЕИ. г СЩЛБАСГ КщЕИ ЭТИ ПЯщПЕИ МА ЖТИАВТЕъ СЕ 10 ЛщЯЕР.
      --,[сулб_глея_ейтекесгс] AS [Symb_Imer_Ektelesis] --inconsequential --аМ АЖАИЯщСЫ АУТч ТГМ ГЛЕЯОЛГМъА АПЭ ТГМ ПЯАЦЛАТИЙч ГЛЕЯОЛГМъА ЕЙТщКЕСГР, БКщПЫ ТГМ ЙАХУСТщЯГСГ
      --,[аявг_паяатгя_ейтекесгс] --inconsequential
      --,[текос_паяатгя_ейтекесгс] --inconsequential
	  --,[KatClientDelay]
	  --,[Kat_Kathisterisi_Pelati]
      --,[йат_йахустеягсг_пекатг] AS [Kat_Kathisterisi_Pelati] --
      --,[йат_вуй_йахустеягсг_пекатг] AS [Kat_Xyk_Kathisterisi_Pelati] --
      --,[йат_емд_йах_пекатг] AS [Kat_End_Kath_Pelati] --аМ ЙэПОИОР НЕЙИМчСЕИ ЛИА ЙАХУСТщЯГСГ ТГМ ГЛЕЯОЛГМъА апо  ЙАИ ДЕМ ЙАТАВЧЯГСГ ТО еыс ТО binary ХА ЦъМЕИ 1 АККэ ДЕМ ХА щВЕИ СЩМОКО ГЛЕЯЧМ ЙАХУСТщЯГСГР. 
      --,[KatDEHDelay]
	  --,[Kat_Kathisterisi_DEH]
	  --,[йат_йахустеягсг_дег] AS [Kat_Kathisterisi_DEH] --
      --,[йат_вуй_йахустеягсг_дег] AS [Kat_Xyk_Kathisterisi_DEH] --
      --,[йат_емд_йах_дег] AS [Kat_End_Kath_DEH] --аМ ЙэПОИОР НЕЙИМчСЕИ ЛИА ЙАХУСТщЯГСГ ТГМ ГЛЕЯОЛГМъА апо  ЙАИ ДЕМ ЙАТАВЧЯГСГ ТО еыс ТО binary ХА ЦъМЕИ 1 АККэ ДЕМ ХА щВЕИ СЩМОКО ГЛЕЯЧМ ЙАХУСТщЯГСГР. 
      --,[KatOthersDelay]
	  --,[Kat_Kathisterisi_Triton]
	  --,[йат_йахустеягсг_тяитым] AS [Kat_Kathisterisi_Triton] --
      --,[йат_вуй_йахустеягсг_тяитым] AS [Kat_Xyk_Kathisterisi_Triton] --
      --,[йат_емд_йах_тяитым] AS [Kat_End_Kath_Triton] --аМ ЙэПОИОР НЕЙИМчСЕИ ЛИА ЙАХУСТщЯГСГ ТГМ ГЛЕЯОЛГМъА апо  ЙАИ ДЕМ ЙАТАВЧЯГСГ ТО еыс ТО binary ХА ЦъМЕИ 1 АККэ ДЕМ ХА щВЕИ СЩМОКО ГЛЕЯЧМ ЙАХУСТщЯГСГР. 
      --,[глея_емаянгс] AS [Imer_Enarksis] --inconsequential
      --,[глея_ейтекесгс] AS [Imer_Ektelesis] --inconsequential --пЭТЕ ЖТИэВТГЙЕ
      --,[пос_ейтекесгс] AS [Pos_Ektelesis] --inconsequential (АМ ПКГЯЧХГЙЕ О ЕЯЦОКэБОР ч ЭВИ)
      --,[пистопоигсг] AS [Pistopiisi] --inconsequential
      --,[глея_пистопоигсгс] AS [Imer_Pistopiisis] --inconsequential
      --,[глеяес_ейтекесгс] AS [Meres_Ektelesis]
      --,[еяц_глеяес_ейтекесгс] AS [Erg_Meres_Ektelesis] 
      --,UPPER(LTRIM(RTRIM([сумеяцеио_йатасйеугс]))) AS [Sinergio_Kataskevis]
      --,UPPER(LTRIM(RTRIM([йатасйеуастгс]))) AS [Kataskevastis]
      --,[апок_йостос_еяцатийым_йатасйеугс] AS [Apol_Kostos_Ergatikon_Kataskevis] --Mostly Missing
      --,[апок_йостос_укийым_йатасйеугс] AS [Apol_Kostos_Ylikon_Kataskevis] --Mostly Missing
      --,[апок_йостос_йатасйеугс] AS [Apol_Kostos_Kataskevis] --Mostly Missing
      --,[ейтупысг_пяытойокоу] AS [Ektiposi_Protokolou] --inconsequential
      --,[омолатепымуло] --inconsequential
      --,[диеухумсг] --inconsequential
      --,[покг] AS [Poli] --Feature Engineered a new City field
      --,[покг_у_с] AS [Poli_Y_S] --Feature Engineered a new City field
      --,[у_с] AS [Y_S] --inconsequential (30,874 Factors)
      --,[тгкежымо] --inconsequential
      --,[паяатгягсеис] --inconsequential
      --,[паяатгягсеис2] --inconsequential
      ,[ейтасг_еяцоу] AS [Ektasi_Ergou]
      ,[амацйг_ус] AS [Anagi_YS]
	  --,[Diktio_Xt_Mt]
      --,[дийтуо_вт_лт] AS [Diktio_Xt_Mt] --inconsequential --One of its 3 factors is a Blank - This is probably meant as a NULL but it is not, so it's featured engineered
      --,[йыд_коцаяиаслоу] AS [Kod_Logariasmou] -- 41/D = епемдусг, 42/M = ейлетакеусг. 219896 41, 105615 42, 18918  D, 21397  M, 11635 NULL
      --,[йыд_амакусгс] AS [Kod_Analysis] -- 1163 Factors, Ipokatigories Ergon
      --,[SAP_ваяайтгяислос_еяцоу] AS [SAP_Xaraktirismos_Ergou] --41/D = епемдусг, 42/M = ейлетакеусг. 219896 41, 105615 42, 18918  D, 21397  M, 11635 NULL
      --,[SAP_сйопос_еяцоу] AS [SAP_Skopos_Ergou] --Ipokatigories Ergon
      ,UPPER(LTRIM(RTRIM([SAP_тупос_пекатг]))) AS [SAP_Typos_Pelati]
      ,UPPER(LTRIM(RTRIM([SAP_еидос_аитглатос]))) AS [SAP_Eidos_Aitimatos]
      --,[SAP_аяихлос_еяцоу] AS [SAP_Arith_Ergou] --inconsequential (89,051 Factors)
      --,[UserName] --inconsequential
      --,[UpDate] --inconsequential
      --,[Test] --inconsequential
	  ,[Kathisterisi_AitisisKataxorisis]
	  ,[Kathisterisi_Meletis]
	  ,[Kathisterisi_Anagelias]
	  ,[DayOfYearSine]
	  ,[DayOfYearCosine]
	  ,[DayOfYearCartesX]
	  ,[DayOfYearCartesY]
	  ,[MarkedForTest]
	  
  FROM [YLIKA_KOSTOL].[dbo].[еяца]
  
    --inconsequential
    --INNER JOIN (SELECT [ID] AS tmp_ID4,
				--	(SELECT CASE
				--		WHEN UPPER(LTRIM(RTRIM([дийтуо_вт_лт]))) = 'вт' OR UPPER(LTRIM(RTRIM([дийтуо_вт_лт]))) = 'лт' THEN
				--			(UPPER(LTRIM(RTRIM([дийтуо_вт_лт]))))
				--		END AS [Diktio_Xt_Mt]
				--	)
				--AS [Diktio_Xt_Mt]
				--FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp4
				--ON tmp4.tmp_ID4 = [еяца].ID
				
    INNER JOIN (SELECT [ID] AS tmp_ID3,
					(SELECT CASE
						WHEN [покг] IS NOT NULL THEN
							(UPPER(LTRIM(RTRIM([покг]))))
						WHEN [покг_у_с] IS NOT NULL THEN
							(UPPER(LTRIM(RTRIM([покг_у_с]))))
						END AS [Onoma_Polis]
					)
				AS [Onoma_Polis]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp3
				ON tmp3.tmp_ID3 = [еяца].ID
				
    INNER JOIN (SELECT [ID] AS tmp_ID0,
					(SELECT CASE
						WHEN [йыд_коцаяиаслоу] = '41'
						OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D'
						OR [SAP_ваяайтгяислос_еяцоу] = '41'
						OR UPPER(LTRIM(RTRIM([SAP_ваяайтгяислос_еяцоу]))) = 'D'
						OR [йатгцояиа] = '9300'
						OR [йатгцояиа] = '9900'
						OR [йатгцояиа] = '9500'
						OR [йатгцояиа] = '9700'
						OR [йатгцояиа] = '9600'
						THEN
							('EPENDISI')
						
						WHEN [йыд_коцаяиаслоу] = '42'
						OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M'
						OR [SAP_ваяайтгяислос_еяцоу] = '42'
						OR UPPER(LTRIM(RTRIM([SAP_ваяайтгяислос_еяцоу]))) = 'M'
						OR [йатгцояиа] = '250'
						THEN
							('EKMETALEFSI')
						END AS [Xaraktirismos_Ergou]
					)
				AS [Xaraktirismos_Ergou]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp0
				ON tmp0.tmp_ID0 = [еяца].ID
				
    INNER JOIN (SELECT [ID] AS tmp_ID1,
					(SELECT CASE
						WHEN UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'EA' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'EB' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'EC' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'ED' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'EE' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'EF' OR
						UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SA' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SB' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SC' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SD' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SE' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SF' OR
						UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SG' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SH' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SI' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SJ' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SK' OR UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))) = 'SL' THEN
							(UPPER(LTRIM(RTRIM([SAP_сйопос_еяцоу]))))
						
						WHEN UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'EA' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'EB' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'EC' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'ED' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'EE' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'EF' OR
						 UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SA' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SB' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SC' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SD' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SE' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SF' OR
						  UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SG' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SH' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SI' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SJ' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SK' OR UPPER(LTRIM(RTRIM([йыд_амакусгс]))) = 'SL' THEN
							(UPPER(LTRIM(RTRIM([йыд_амакусгс]))))
						--Placement DOES matter hereinafter as some rules are more general than others and less general rules will never occur if they aren't placed first
						WHEN ([йыд_коцаяиаслоу] = '41' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D') AND [йыд_амакусгс] LIKE '324%' THEN
							'EB'
						WHEN ([йыд_коцаяиаслоу] = '41' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D') AND [йыд_амакусгс] LIKE '32%' THEN
							'EA'
						WHEN ([йыд_коцаяиаслоу] = '41' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D') AND [йыд_амакусгс] LIKE '336%' THEN
							'EF'
						WHEN ([йыд_коцаяиаслоу] = '41' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D') AND [йыд_амакусгс] LIKE '33%' THEN
							'EC'
						WHEN ([йыд_коцаяиаслоу] = '41' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D') AND [йыд_амакусгс] LIKE '316%' THEN
							'ED'
						WHEN ([йыд_коцаяиаслоу] = '41' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D') AND [йыд_амакусгс] LIKE '34%' THEN
							'EE'
						WHEN [йатгцояиа] = '9300' THEN
							'EA'
						WHEN [йатгцояиа] = '9900' THEN
							'EB'
						WHEN [йатгцояиа] = '9500' THEN
							'EC'
						WHEN [йатгцояиа] = '9700' THEN
							'ED'
						WHEN [йатгцояиа] = '9600' THEN
							'EF'
						WHEN [йатгцояиа] = '9400' AND ([йыд_коцаяиаслоу] = '41'
														OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'D'
														OR [SAP_ваяайтгяислос_еяцоу] = '41'
														OR UPPER(LTRIM(RTRIM([SAP_ваяайтгяислос_еяцоу]))) = 'D') THEN
							'EE'


						WHEN ([йыд_коцаяиаслоу] = '42' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M') AND [йыд_амакусгс] LIKE '321%' THEN
							'SA'
						WHEN ([йыд_коцаяиаслоу] = '42' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M') AND [йыд_амакусгс] LIKE '322%' THEN
							'SB'
						WHEN ([йыд_коцаяиаслоу] = '42' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M') AND [йыд_амакусгс] LIKE '33%' THEN
							'SC'
						WHEN ([йыд_коцаяиаслоу] = '42' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M') AND [йыд_амакусгс] LIKE '36%' THEN
							'SF'
						WHEN ([йыд_коцаяиаслоу] = '42' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M') AND [йыд_амакусгс] LIKE '325%' THEN
							'SG'
						WHEN ([йыд_коцаяиаслоу] = '42' OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M') AND [йыд_амакусгс] LIKE '326%' THEN
							'SH'
						WHEN [йатгцояиа] = '9400' AND ([йыд_коцаяиаслоу] = '42'
														OR UPPER(LTRIM(RTRIM([йыд_коцаяиаслоу]))) = 'M'
														OR [SAP_ваяайтгяислос_еяцоу] = '42'
														OR UPPER(LTRIM(RTRIM([SAP_ваяайтгяислос_еяцоу]))) = 'M') THEN
							'SF'
						END AS [Skopos_Ergou]
					)
				AS [Skopos_Ergou]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp1
				ON tmp1.tmp_ID1 = [еяца].ID
				
	--INNER JOIN (SELECT ID as tmpID7, (SELECT CASE
	--											WHEN [глея_лекетгс] IS NOT NULL THEN
	--												[глея_лекетгс]
	--											ELSE
	--												[глея_амаццекиас]
	--											END AS [Hmerominia]
	--											) [Hmerominia]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp7
	--			ON tmp7.tmpID7 = [еяца].ID

	INNER JOIN (SELECT ID AS tmpID, (convert(varchar(4), YEAR([глея_аитгсгс])) +  (SELECT CASE 
																						WHEN MONTH([глея_аитгсгс]) < 4 THEN
																							(' Q1')
																						WHEN MONTH([глея_аитгсгс]) < 7 THEN
																							(' Q2')
																						WHEN MONTH([глея_аитгсгс]) < 10 THEN
																							(' Q3')
																						ELSE
																							(' Q4')
																						END AS [TimeSeriesDate]
																					)
								        ) AS [TimeSeriesDate]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp
				ON tmp.tmpID = [еяца].ID

	INNER JOIN (SELECT [ID] AS tmpID2,
					(SELECT CASE
						WHEN [глея_упоцяажгс] IS NULL THEN
							(0)
						ELSE
							(1)
						END AS [Label]
					) AS [Label]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp2
				ON tmp2.tmpID2 = [еяца].ID

	INNER JOIN (SELECT [ID] AS tmpID6,
					(SELECT CASE
						WHEN (([лек_йахустеягсг_пекатг] IS NULL OR [лек_йахустеягсг_пекатг] = 0)
						AND ([лек_вуй_йахустеягсг_пекатг] IS NULL OR [лек_вуй_йахустеягсг_пекатг] = 0)
						AND [лек_емд_йах_пекатг] = 0) THEN
							(0)
						ELSE
							(1)
						END AS [MelClientDelay]
					) AS [MelClientDelay]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp6
				ON tmp6.tmpID6 = [еяца].ID
				
	INNER JOIN (SELECT [ID] AS tmpID5,
					(SELECT CASE
						WHEN (([лек_йахустеягсг_дег] IS NULL OR [лек_йахустеягсг_дег] = 0)
						AND ([лек_вуй_йахустеягсг_дег] IS NULL OR [лек_вуй_йахустеягсг_дег] = 0)
						AND [лек_емд_йах_дег] = 0) THEN
							(0)
						ELSE
							(1)
						END AS [MelDEHDelay]
					) AS [MelDEHDelay]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp5
				ON tmp5.tmpID5 = [еяца].ID
				
	INNER JOIN (SELECT [ID] AS tmpID8,
					(SELECT CASE
						WHEN (([лек_йахустеягсг_тяитым] IS NULL OR [лек_йахустеягсг_тяитым] = 0)
						AND ([лек_вуй_йахустеягсг_тяитым] IS NULL OR [лек_вуй_йахустеягсг_тяитым] = 0)
						AND [лек_емд_йах_тяитым] = 0) THEN
							(0)
						ELSE
							(1)
						END AS [MelOthersDelay]
					) AS [MelOthersDelay]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp8
				ON tmp8.tmpID8 = [еяца].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID9,
	--				(SELECT CASE
	--					WHEN (([йат_йахустеягсг_пекатг] IS NULL OR [йат_йахустеягсг_пекатг] = 0)
	--					AND ([йат_вуй_йахустеягсг_пекатг] IS NULL OR [йат_вуй_йахустеягсг_пекатг] = 0)
	--					AND [йат_емд_йах_пекатг] = 0) THEN
	--						(0)
	--					ELSE
	--						(1)
	--					END AS [KatClientDelay]
	--				) AS [KatClientDelay]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp9
	--			ON tmp9.tmpID9 = [еяца].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID10,
	--				(SELECT CASE
	--					WHEN (([йат_йахустеягсг_дег] IS NULL OR [йат_йахустеягсг_дег] = 0)
	--					AND ([йат_вуй_йахустеягсг_дег] IS NULL OR [йат_вуй_йахустеягсг_дег] = 0)
	--					AND [йат_емд_йах_дег] = 0) THEN
	--						(0)
	--					ELSE
	--						(1)
	--					END AS [KatDEHDelay]
	--				) AS [KatDEHDelay]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp10
	--			ON tmp10.tmpID10 = [еяца].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID11,
	--				(SELECT CASE
	--					WHEN (([йат_йахустеягсг_тяитым] IS NULL OR [йат_йахустеягсг_тяитым] = 0)
	--					AND ([йат_вуй_йахустеягсг_тяитым] IS NULL OR [йат_вуй_йахустеягсг_тяитым] = 0)
	--					AND [йат_емд_йах_тяитым] = 0) THEN
	--						(0)
	--					ELSE
	--						(1)
	--					END AS [KatOthersDelay]
	--				) AS [KatOthersDelay]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp11
	--			ON tmp11.tmpID11 = [еяца].ID
				
	INNER JOIN (SELECT [ID] AS tmpID12,
					(SELECT CASE
						WHEN ([лек_йахустеягсг_пекатг] IS NOT NULL) THEN
							([лек_йахустеягсг_пекатг])
						WHEN ([лек_вуй_йахустеягсг_пекатг] IS NOT NULL) THEN
							([лек_вуй_йахустеягсг_пекатг])
						ELSE
							(0)
						END AS [Mel_Kathisterisi_Pelati]
					) AS [Mel_Kathisterisi_Pelati]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp12
				ON tmp12.tmpID12 = [еяца].ID
				
	INNER JOIN (SELECT [ID] AS tmpID13,
					(SELECT CASE
						WHEN ([лек_йахустеягсг_дег] IS NOT NULL) THEN
							([лек_йахустеягсг_дег])
						WHEN ([лек_вуй_йахустеягсг_дег] IS NOT NULL) THEN
							([лек_вуй_йахустеягсг_дег])
						ELSE
							(0)
						END AS [Mel_Kathisterisi_DEH]
					) AS [Mel_Kathisterisi_DEH]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp13
				ON tmp13.tmpID13 = [еяца].ID

	INNER JOIN (SELECT [ID] AS tmpID14,
					(SELECT CASE
						WHEN ([лек_йахустеягсг_тяитым] IS NOT NULL) THEN
							([лек_йахустеягсг_тяитым])
						WHEN ([лек_вуй_йахустеягсг_тяитым] IS NOT NULL) THEN
							([лек_вуй_йахустеягсг_тяитым])
						ELSE
							(0)
						END AS [Mel_Kathisterisi_Triton]
					) AS [Mel_Kathisterisi_Triton]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp14
				ON tmp14.tmpID14 = [еяца].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID15,
	--				(SELECT CASE
	--					WHEN ([йат_йахустеягсг_пекатг] IS NOT NULL) THEN
	--						([йат_йахустеягсг_пекатг])
	--					ELSE
	--						(0)
	--					END AS [Kat_Kathisterisi_Pelati]
	--				) AS [Kat_Kathisterisi_Pelati]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp15
	--			ON tmp15.tmpID15 = [еяца].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID16,
	--				(SELECT CASE
	--					WHEN ([йат_йахустеягсг_дег] IS NOT NULL) THEN
	--						([йат_йахустеягсг_дег])
	--					ELSE
	--						(0)
	--					END AS [Kat_Kathisterisi_DEH]
	--				) AS [Kat_Kathisterisi_DEH]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp16
	--			ON tmp16.tmpID16 = [еяца].ID

	--INNER JOIN (SELECT [ID] AS tmpID17,
	--				(SELECT CASE
	--					WHEN ([йат_йахустеягсг_тяитым] IS NOT NULL) THEN
	--						([йат_йахустеягсг_тяитым])
	--					ELSE
	--						(0)
	--					END AS [Kat_Kathisterisi_Triton]
	--				) AS [Kat_Kathisterisi_Triton]
	--			FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp17
	--			ON tmp17.tmpID17 = [еяца].ID
					
	INNER JOIN (SELECT [ID] AS tmpID24, DATEDIFF(day, [глея_аитгсгс], [глея_йатавыягсгс]) AS [Kathisterisi_AitisisKataxorisis]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp24
				ON tmp24.tmpID24 = [еяца].ID
				
	INNER JOIN (SELECT [ID] AS tmpID25, DATEDIFF(day, [глея_йатавыягсгс], [глея_лекетгс]) AS [Kathisterisi_Meletis]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp25
				ON tmp25.tmpID25 = [еяца].ID
				
	INNER JOIN (SELECT [ID] AS tmpID22, DATEDIFF(day, [глея_лекетгс], [глея_амаццекиас]) AS [Kathisterisi_Anagelias]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp22
				ON tmp22.tmpID22 = [еяца].ID

	INNER JOIN (SELECT ID AS tmpID26, SIN(((datepart(dayofyear, [глея_аитгсгс])) * 360) / 366) AS [DayOfYearSine]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp26
				ON tmp26.tmpID26 = [еяца].ID
				
	INNER JOIN (SELECT ID AS tmpID27, COS(((datepart(dayofyear, [глея_аитгсгс])) * 360) / 366) AS [DayOfYearCosine]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp27
				ON tmp27.tmpID27 = [еяца].ID
				
	INNER JOIN (SELECT ID AS tmpID28, (datepart(year, [глея_аитгсгс]) * COS(((datepart(dayofyear, [глея_аитгсгс])) * 360) / 366)) AS [DayOfYearCartesX]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp28
				ON tmp28.tmpID28 = [еяца].ID

	INNER JOIN (SELECT ID AS tmpID29, (datepart(year, [глея_аитгсгс]) * SIN(((datepart(dayofyear, [глея_аитгсгс])) * 360) / 366)) AS [DayOfYearCartesY]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp29
				ON tmp29.tmpID29 = [еяца].ID

	INNER JOIN (SELECT [ID] AS tmpID30,
		(SELECT CASE
			WHEN ([Label] = 0 AND DATEADD(MONTH,2,[глея_аитгсгс]) >= '2016-07-06') THEN
				(1)
			ELSE
				(0)
			END AS [MarkedForTest]
		) AS [MarkedForTest]
	FROM [YLIKA_KOSTOL].[dbo].[еяца]
		INNER JOIN (SELECT [ID] AS tmpID31,
					(SELECT CASE
						WHEN [глея_упоцяажгс] IS NULL THEN
							(0)
						ELSE
							(1)
						END AS [Label]
					) AS [Label]
				FROM [YLIKA_KOSTOL].[dbo].[еяца]) tmp31
				ON tmp31.tmpID31 = [еяца].ID) tmp30
	ON tmp30.tmpID30 = [еяца].ID
															
  WHERE (([дег_пекатгс] <> 1 OR [дег_пекатгс] IS NULL) AND  --Getting only Clients --1=DEH
		 ([айуяыхем] = 0 OR [айуяыхем] IS NULL) AND  --Akyrothen <> 0 = false record, ergo non needed
		 [Onoma_Polis] IS NOT NULL AND --216171 --Needed for the Clustering
		 --([Hmerominia] IS NOT NULL) AND --NOT Null so that we can have an accurate Label --The difference between those dates is negligible, so if the one is NULL, the other is used
		 
		 --For the Real-data algorithm, we need the whole thing to pass through Clustering, and this clause will be applied afterwards programmatically
		 --([Label] = 0 AND DATEADD(MONTH,2,[глея_аитгсгс]) >= '2016-07-06') AND --[Instead of '2016-07-06', it should be GETDATE() on the real HEDNO's server] --Getting the as of yet undecided projects
		 
		 [Xaraktirismos_Ergou] IS NOT NULL AND --The database encompasses a wide range of things but we only care for projects. Those are the 41,D,42,M. The NOT NULL means that that's all the query retrieves
		 [Skopos_Ergou] IS NOT NULL AND --The feature's been engineered so that only rows with a value on this are relevant
		 
		 [глеяес_лекетгс] >= 0 AND --A study cannot have ended before it even began, hence erroneous data (noise) are being cleaned
		 [йостос_еяцатийым_йатасйеугс] IS NOT NULL AND --Cost is a critical variable, so it must be filled in
		 [йостос_укийым_йатасйеугс] IS NOT NULL AND --Cost is a critical variable, so it must be filled in
		 [йостос_йатасйеугс] IS NOT NULL AND --Cost is a critical variable, so it must be filled in
		 [йостос_еяцокабийым_епидосгс] IS NOT NULL AND --Cost is a critical variable, so it must be filled in

		 [йостос_еяцатийым_йатасйеугс] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects
		 [йостос_укийым_йатасйеугс] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects
		 [йостос_йатасйеугс] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects
		 [йостос_еяцокабийым_епидосгс] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects

		 [MONADA] IS NOT NULL AND
		 [йатгцояиа] IS NOT NULL AND
		 [глеяес_лекетгс] IS NOT NULL AND
		 [сумеяцеио_лекетгс] IS NOT NULL AND
		 [ейтасг_еяцоу] IS NOT NULL AND
		 [амацйг_ус] IS NOT NULL AND

		 [глея_аитгсгс] IS NOT NULL AND
		 [глея_йатавыягсгс] IS NOT NULL AND
		 [глея_амаццекиас] IS NOT NULL AND
		 [глея_лекетгс] IS NOT NULL --AND

		 --[SAP_тупос_пекатг] IS NOT NULL AND
		 --[SAP_еидос_аитглатос] IS NOT NULL --AND
		 )