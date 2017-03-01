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
	  --,[Hmerominia] --����� ������� � ����_�������, ������� ���� ��� ����, ������ ������� ��� ���� ��� ����_���������� - ���������� ����� ��� ���� � ���������� �������, ���� �� �������� NULL
      --,[����_�������] AS [Imer_Meletis] --���� � ������� �� ������ ������ --������� ���� ��� ���������� ��� ���� ��������� � ������
      --,[����_����������] AS [Imer_Anagelias] --��������� ������ ��� ������ ����� ����� ��� ��� ���� �� ���� ��� ���������� ��� ������ �� �������� ��� �� ���� (������ �����, ���� ���������� �� ����_�������)
      --,[����_���������] AS [Imer_Ypografis] --LABELED DATA: ����� ����� ��� ������ �� ����������� ���. �� �� ����� � �������� ��� ����. ��� �� ������ ��� �� ����� ������� ���� (date) ������������ �� �����
      --,[ID2] --inconsequential
      --,[ID_��������] AS [ID_Protasis] --inconsequential
      --,[����] AS [Etos] --Subsumed under the TimeSeriesDate paradigm --inconsequential
      --,[�_�] AS [A_A] --inconsequential
      --,[����_�����������] AS [Imer_Kataxorisis] --inconsequential
      --,[�������] AS [Arithmos] --inconsequential
      --,[�������������_�����] --inconsequential --Completely Missing
      --,[������_�����] --inconsequential --Completely Missing
      --,[����_�����] --inconsequential --Completely Missing
      --,[�������_�����] --inconsequential --Completely Missing
      --,[��������] AS [Akyrothen] --Rows cannot be deleted, hence if the need arises due to an erroneous entry, it's reflected here
      ,[���������] AS [Katigoria] --Similar to [SAP_�������������_�����] but more general
	  ,[Xaraktirismos_Ergou] --Main project's category
	  ,[Skopos_Ergou] --Project's Sub-category
      --,[���_�����] AS [FOP_Lipa] --inconsequential
      --,[�����_��������] AS [Zimia_Paralagi] --inconsequential
      --,[�����] AS [Omada] --inconsequential
      --,[����_�������] AS [Etos_Meletis] --inconsequential
      --,[�������_�������] AS [Arithmos_Meletis] --inconsequential
      --,[�����_������������0] AS [Eidos_Eksipiretisis0] --inconsequential
      --,[����_����������] AS [Etos_Kataskevis] --inconsequential
      --,[�������_����������] AS [Arithmos_Kataskevis] --inconsequential
      --,[���_�������] AS [DEH_Pelatis] -- 1=DEH, 0,2,NULL = Client --We ONLY care about the Clients, not DEH
      --,[��_����������_������] AS [Ar_Protokolou_Pelati]  --inconsequential --Mostly Missing --inconsequential
      --,[��_����������_���] AS [Ar_Protokolou_DEH] --inconsequential --Mostly Missing --inconsequential
      --,[���_���������] --Mostly Missing --inconsequential
      --,[��_�������] AS [Ar_Paroxis] --Mostly Missing --inconsequential
      --,[���������] --inconsequential
      --,[����_�������] AS [Imer_Aitisis] --inconsequential
      --,[����_�������_�������] --Mostly Missing --inconsequential
      --,[�����_�������_�������] --Mostly Missing --inconsequential
	  ,[MelClientDelay] --Whether there was a delay in the study caused by the client
	  ,[Mel_Kathisterisi_Pelati] --The delay, in days, caused by the client
      --,[���_�����������_������] AS [Mel_Kathisterisi_Pelati] --
      --,[���_���_�����������_������] AS [Mel_Xik_Kathisterisi_Pelati] --
      --,[���_���_���_������] AS [Mel_End_Kath_Pelati] --�� ������� ��������� ��� ����������� ��� ���������� ���  ��� ��� ���������� �� ��� �� binary �� ����� 1 ���� ��� �� ���� ������ ������ ������������. 
	  ,[MelDEHDelay]
	  ,[Mel_Kathisterisi_DEH] --The delay, in days, caused by the DEDDHE
      --,[���_�����������_���] AS [Mel_Kathisterisi_DEH] --Should facilitate feature engineer with "XYK" and "END"
      --,[���_���_�����������_���] AS [Mel_Xyk_Kathisterisi_DEH] --Should facilitate feature engineer with "XYK" and "END"
      --,[���_���_���_���] AS [Mel_End_Kath_DEH] --�� ������� ��������� ��� ����������� ��� ���������� ���  ��� ��� ���������� �� ��� �� binary �� ����� 1 ���� ��� �� ���� ������ ������ ������������. 
      ,[MelOthersDelay]
	  ,[Mel_Kathisterisi_Triton]
	  --,[���_�����������_������] AS [Mel_Kathisterisi_Triton] --Should facilitate feature engineer with "XYK" and "END"
      --,[���_���_�����������_������] AS [Mel_Xyk_Kathisterisi_Triton] --Should facilitate feature engineer with "XYK" and "END"
      --,[���_���_���_������] AS [Mel_End_Kath_Triton] --�� ������� ��������� ��� ����������� ��� ���������� ���  ��� ��� ���������� �� ��� �� binary �� ����� 1 ���� ��� �� ���� ������ ������ ������������. 
      ,[������_�������] AS [Meres_Meletis] --The number of days that the 'Study' part of the project lasted, including non-DEDEH delays
      --,[���_������_�������] AS [Erg_Meres_Meletis] --We already have the Days it took
      ,UPPER(LTRIM(RTRIM([���������_�������]))) AS [Sinergio_Meletis] --To sinergio, 3 Factor Levels
      --,UPPER(LTRIM(RTRIM([���������]))) AS [Meletitis] --1932 Meletites
      --,[��_����������]
      --,[������_��������] AS [Kostos_Meletiti] --Mostly Missing
      ,[������_���������_����������] AS [Kostos_Ergatikon_Kataskevis]
      ,[������_������_����������] AS [Kostos_Ilikon_Kataskevis]
      ,[������_����������] AS [Kostos_Kataskevis]
      ,[������_�����������_��������] AS [Kostos_Ergolavikon_Epidosis]
      --,[��_�������] --inconsequential
      --,[����_���������] --inconsequential --���� ���� ���� ��������
      --,[�����_������������] AS [Idos_Eksipiretisis] --inconsequential
      --,[������_�����] --inconsequential
      --,[����_����_�������] AS [Symb_Imer_Enarksis] --inconsequential --��������� ����������. ��������� �����. ������� ��� ������� ���� �� �� �������. � ������� ���� ��� ������ �� �������� �� 10 �����.
      --,[����_����_���������] AS [Symb_Imer_Ektelesis] --inconsequential --�� �������� ���� ��� ���������� ��� ��� ���������� ���������� ���������, ����� ��� �����������
      --,[����_�������_���������] --inconsequential
      --,[�����_�������_���������] --inconsequential
	  --,[KatClientDelay]
	  --,[Kat_Kathisterisi_Pelati]
      --,[���_�����������_������] AS [Kat_Kathisterisi_Pelati] --
      --,[���_���_�����������_������] AS [Kat_Xyk_Kathisterisi_Pelati] --
      --,[���_���_���_������] AS [Kat_End_Kath_Pelati] --�� ������� ��������� ��� ����������� ��� ���������� ���  ��� ��� ���������� �� ��� �� binary �� ����� 1 ���� ��� �� ���� ������ ������ ������������. 
      --,[KatDEHDelay]
	  --,[Kat_Kathisterisi_DEH]
	  --,[���_�����������_���] AS [Kat_Kathisterisi_DEH] --
      --,[���_���_�����������_���] AS [Kat_Xyk_Kathisterisi_DEH] --
      --,[���_���_���_���] AS [Kat_End_Kath_DEH] --�� ������� ��������� ��� ����������� ��� ���������� ���  ��� ��� ���������� �� ��� �� binary �� ����� 1 ���� ��� �� ���� ������ ������ ������������. 
      --,[KatOthersDelay]
	  --,[Kat_Kathisterisi_Triton]
	  --,[���_�����������_������] AS [Kat_Kathisterisi_Triton] --
      --,[���_���_�����������_������] AS [Kat_Xyk_Kathisterisi_Triton] --
      --,[���_���_���_������] AS [Kat_End_Kath_Triton] --�� ������� ��������� ��� ����������� ��� ���������� ���  ��� ��� ���������� �� ��� �� binary �� ����� 1 ���� ��� �� ���� ������ ������ ������������. 
      --,[����_�������] AS [Imer_Enarksis] --inconsequential
      --,[����_���������] AS [Imer_Ektelesis] --inconsequential --���� ���������
      --,[���_���������] AS [Pos_Ektelesis] --inconsequential (�� ��������� � ��������� � ���)
      --,[�����������] AS [Pistopiisi] --inconsequential
      --,[����_������������] AS [Imer_Pistopiisis] --inconsequential
      --,[������_���������] AS [Meres_Ektelesis]
      --,[���_������_���������] AS [Erg_Meres_Ektelesis] 
      --,UPPER(LTRIM(RTRIM([���������_����������]))) AS [Sinergio_Kataskevis]
      --,UPPER(LTRIM(RTRIM([�������������]))) AS [Kataskevastis]
      --,[����_������_���������_����������] AS [Apol_Kostos_Ergatikon_Kataskevis] --Mostly Missing
      --,[����_������_������_����������] AS [Apol_Kostos_Ylikon_Kataskevis] --Mostly Missing
      --,[����_������_����������] AS [Apol_Kostos_Kataskevis] --Mostly Missing
      --,[��������_����������] AS [Ektiposi_Protokolou] --inconsequential
      --,[�������������] --inconsequential
      --,[���������] --inconsequential
      --,[����] AS [Poli] --Feature Engineered a new City field
      --,[����_�_�] AS [Poli_Y_S] --Feature Engineered a new City field
      --,[�_�] AS [Y_S] --inconsequential (30,874 Factors)
      --,[��������] --inconsequential
      --,[������������] --inconsequential
      --,[������������2] --inconsequential
      ,[������_�����] AS [Ektasi_Ergou]
      ,[������_��] AS [Anagi_YS]
	  --,[Diktio_Xt_Mt]
      --,[������_��_��] AS [Diktio_Xt_Mt] --inconsequential --One of its 3 factors is a Blank - This is probably meant as a NULL but it is not, so it's featured engineered
      --,[���_�����������] AS [Kod_Logariasmou] -- 41/D = ��������, 42/M = �����������. 219896 41, 105615 42, 18918  D, 21397  M, 11635 NULL
      --,[���_��������] AS [Kod_Analysis] -- 1163 Factors, Ipokatigories Ergon
      --,[SAP_�������������_�����] AS [SAP_Xaraktirismos_Ergou] --41/D = ��������, 42/M = �����������. 219896 41, 105615 42, 18918  D, 21397  M, 11635 NULL
      --,[SAP_������_�����] AS [SAP_Skopos_Ergou] --Ipokatigories Ergon
      ,UPPER(LTRIM(RTRIM([SAP_�����_������]))) AS [SAP_Typos_Pelati]
      ,UPPER(LTRIM(RTRIM([SAP_�����_���������]))) AS [SAP_Eidos_Aitimatos]
      --,[SAP_�������_�����] AS [SAP_Arith_Ergou] --inconsequential (89,051 Factors)
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
	  
  FROM [YLIKA_KOSTOL].[dbo].[����]
  
    --inconsequential
    --INNER JOIN (SELECT [ID] AS tmp_ID4,
				--	(SELECT CASE
				--		WHEN UPPER(LTRIM(RTRIM([������_��_��]))) = '��' OR UPPER(LTRIM(RTRIM([������_��_��]))) = '��' THEN
				--			(UPPER(LTRIM(RTRIM([������_��_��]))))
				--		END AS [Diktio_Xt_Mt]
				--	)
				--AS [Diktio_Xt_Mt]
				--FROM [YLIKA_KOSTOL].[dbo].[����]) tmp4
				--ON tmp4.tmp_ID4 = [����].ID
				
    INNER JOIN (SELECT [ID] AS tmp_ID3,
					(SELECT CASE
						WHEN [����] IS NOT NULL THEN
							(UPPER(LTRIM(RTRIM([����]))))
						WHEN [����_�_�] IS NOT NULL THEN
							(UPPER(LTRIM(RTRIM([����_�_�]))))
						END AS [Onoma_Polis]
					)
				AS [Onoma_Polis]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp3
				ON tmp3.tmp_ID3 = [����].ID
				
    INNER JOIN (SELECT [ID] AS tmp_ID0,
					(SELECT CASE
						WHEN [���_�����������] = '41'
						OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D'
						OR [SAP_�������������_�����] = '41'
						OR UPPER(LTRIM(RTRIM([SAP_�������������_�����]))) = 'D'
						OR [���������] = '9300'
						OR [���������] = '9900'
						OR [���������] = '9500'
						OR [���������] = '9700'
						OR [���������] = '9600'
						THEN
							('EPENDISI')
						
						WHEN [���_�����������] = '42'
						OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M'
						OR [SAP_�������������_�����] = '42'
						OR UPPER(LTRIM(RTRIM([SAP_�������������_�����]))) = 'M'
						OR [���������] = '250'
						THEN
							('EKMETALEFSI')
						END AS [Xaraktirismos_Ergou]
					)
				AS [Xaraktirismos_Ergou]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp0
				ON tmp0.tmp_ID0 = [����].ID
				
    INNER JOIN (SELECT [ID] AS tmp_ID1,
					(SELECT CASE
						WHEN UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'EA' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'EB' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'EC' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'ED' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'EE' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'EF' OR
						UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SA' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SB' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SC' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SD' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SE' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SF' OR
						UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SG' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SH' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SI' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SJ' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SK' OR UPPER(LTRIM(RTRIM([SAP_������_�����]))) = 'SL' THEN
							(UPPER(LTRIM(RTRIM([SAP_������_�����]))))
						
						WHEN UPPER(LTRIM(RTRIM([���_��������]))) = 'EA' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'EB' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'EC' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'ED' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'EE' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'EF' OR
						 UPPER(LTRIM(RTRIM([���_��������]))) = 'SA' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SB' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SC' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SD' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SE' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SF' OR
						  UPPER(LTRIM(RTRIM([���_��������]))) = 'SG' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SH' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SI' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SJ' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SK' OR UPPER(LTRIM(RTRIM([���_��������]))) = 'SL' THEN
							(UPPER(LTRIM(RTRIM([���_��������]))))
						--Placement DOES matter hereinafter as some rules are more general than others and less general rules will never occur if they aren't placed first
						WHEN ([���_�����������] = '41' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D') AND [���_��������] LIKE '324%' THEN
							'EB'
						WHEN ([���_�����������] = '41' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D') AND [���_��������] LIKE '32%' THEN
							'EA'
						WHEN ([���_�����������] = '41' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D') AND [���_��������] LIKE '336%' THEN
							'EF'
						WHEN ([���_�����������] = '41' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D') AND [���_��������] LIKE '33%' THEN
							'EC'
						WHEN ([���_�����������] = '41' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D') AND [���_��������] LIKE '316%' THEN
							'ED'
						WHEN ([���_�����������] = '41' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D') AND [���_��������] LIKE '34%' THEN
							'EE'
						WHEN [���������] = '9300' THEN
							'EA'
						WHEN [���������] = '9900' THEN
							'EB'
						WHEN [���������] = '9500' THEN
							'EC'
						WHEN [���������] = '9700' THEN
							'ED'
						WHEN [���������] = '9600' THEN
							'EF'
						WHEN [���������] = '9400' AND ([���_�����������] = '41'
														OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'D'
														OR [SAP_�������������_�����] = '41'
														OR UPPER(LTRIM(RTRIM([SAP_�������������_�����]))) = 'D') THEN
							'EE'


						WHEN ([���_�����������] = '42' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M') AND [���_��������] LIKE '321%' THEN
							'SA'
						WHEN ([���_�����������] = '42' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M') AND [���_��������] LIKE '322%' THEN
							'SB'
						WHEN ([���_�����������] = '42' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M') AND [���_��������] LIKE '33%' THEN
							'SC'
						WHEN ([���_�����������] = '42' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M') AND [���_��������] LIKE '36%' THEN
							'SF'
						WHEN ([���_�����������] = '42' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M') AND [���_��������] LIKE '325%' THEN
							'SG'
						WHEN ([���_�����������] = '42' OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M') AND [���_��������] LIKE '326%' THEN
							'SH'
						WHEN [���������] = '9400' AND ([���_�����������] = '42'
														OR UPPER(LTRIM(RTRIM([���_�����������]))) = 'M'
														OR [SAP_�������������_�����] = '42'
														OR UPPER(LTRIM(RTRIM([SAP_�������������_�����]))) = 'M') THEN
							'SF'
						END AS [Skopos_Ergou]
					)
				AS [Skopos_Ergou]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp1
				ON tmp1.tmp_ID1 = [����].ID
				
	--INNER JOIN (SELECT ID as tmpID7, (SELECT CASE
	--											WHEN [����_�������] IS NOT NULL THEN
	--												[����_�������]
	--											ELSE
	--												[����_����������]
	--											END AS [Hmerominia]
	--											) [Hmerominia]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp7
	--			ON tmp7.tmpID7 = [����].ID

	INNER JOIN (SELECT ID AS tmpID, (convert(varchar(4), YEAR([����_�������])) +  (SELECT CASE 
																						WHEN MONTH([����_�������]) < 4 THEN
																							(' Q1')
																						WHEN MONTH([����_�������]) < 7 THEN
																							(' Q2')
																						WHEN MONTH([����_�������]) < 10 THEN
																							(' Q3')
																						ELSE
																							(' Q4')
																						END AS [TimeSeriesDate]
																					)
								        ) AS [TimeSeriesDate]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp
				ON tmp.tmpID = [����].ID

	INNER JOIN (SELECT [ID] AS tmpID2,
					(SELECT CASE
						WHEN [����_���������] IS NULL THEN
							(0)
						ELSE
							(1)
						END AS [Label]
					) AS [Label]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp2
				ON tmp2.tmpID2 = [����].ID

	INNER JOIN (SELECT [ID] AS tmpID6,
					(SELECT CASE
						WHEN (([���_�����������_������] IS NULL OR [���_�����������_������] = 0)
						AND ([���_���_�����������_������] IS NULL OR [���_���_�����������_������] = 0)
						AND [���_���_���_������] = 0) THEN
							(0)
						ELSE
							(1)
						END AS [MelClientDelay]
					) AS [MelClientDelay]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp6
				ON tmp6.tmpID6 = [����].ID
				
	INNER JOIN (SELECT [ID] AS tmpID5,
					(SELECT CASE
						WHEN (([���_�����������_���] IS NULL OR [���_�����������_���] = 0)
						AND ([���_���_�����������_���] IS NULL OR [���_���_�����������_���] = 0)
						AND [���_���_���_���] = 0) THEN
							(0)
						ELSE
							(1)
						END AS [MelDEHDelay]
					) AS [MelDEHDelay]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp5
				ON tmp5.tmpID5 = [����].ID
				
	INNER JOIN (SELECT [ID] AS tmpID8,
					(SELECT CASE
						WHEN (([���_�����������_������] IS NULL OR [���_�����������_������] = 0)
						AND ([���_���_�����������_������] IS NULL OR [���_���_�����������_������] = 0)
						AND [���_���_���_������] = 0) THEN
							(0)
						ELSE
							(1)
						END AS [MelOthersDelay]
					) AS [MelOthersDelay]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp8
				ON tmp8.tmpID8 = [����].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID9,
	--				(SELECT CASE
	--					WHEN (([���_�����������_������] IS NULL OR [���_�����������_������] = 0)
	--					AND ([���_���_�����������_������] IS NULL OR [���_���_�����������_������] = 0)
	--					AND [���_���_���_������] = 0) THEN
	--						(0)
	--					ELSE
	--						(1)
	--					END AS [KatClientDelay]
	--				) AS [KatClientDelay]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp9
	--			ON tmp9.tmpID9 = [����].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID10,
	--				(SELECT CASE
	--					WHEN (([���_�����������_���] IS NULL OR [���_�����������_���] = 0)
	--					AND ([���_���_�����������_���] IS NULL OR [���_���_�����������_���] = 0)
	--					AND [���_���_���_���] = 0) THEN
	--						(0)
	--					ELSE
	--						(1)
	--					END AS [KatDEHDelay]
	--				) AS [KatDEHDelay]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp10
	--			ON tmp10.tmpID10 = [����].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID11,
	--				(SELECT CASE
	--					WHEN (([���_�����������_������] IS NULL OR [���_�����������_������] = 0)
	--					AND ([���_���_�����������_������] IS NULL OR [���_���_�����������_������] = 0)
	--					AND [���_���_���_������] = 0) THEN
	--						(0)
	--					ELSE
	--						(1)
	--					END AS [KatOthersDelay]
	--				) AS [KatOthersDelay]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp11
	--			ON tmp11.tmpID11 = [����].ID
				
	INNER JOIN (SELECT [ID] AS tmpID12,
					(SELECT CASE
						WHEN ([���_�����������_������] IS NOT NULL) THEN
							([���_�����������_������])
						WHEN ([���_���_�����������_������] IS NOT NULL) THEN
							([���_���_�����������_������])
						ELSE
							(0)
						END AS [Mel_Kathisterisi_Pelati]
					) AS [Mel_Kathisterisi_Pelati]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp12
				ON tmp12.tmpID12 = [����].ID
				
	INNER JOIN (SELECT [ID] AS tmpID13,
					(SELECT CASE
						WHEN ([���_�����������_���] IS NOT NULL) THEN
							([���_�����������_���])
						WHEN ([���_���_�����������_���] IS NOT NULL) THEN
							([���_���_�����������_���])
						ELSE
							(0)
						END AS [Mel_Kathisterisi_DEH]
					) AS [Mel_Kathisterisi_DEH]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp13
				ON tmp13.tmpID13 = [����].ID

	INNER JOIN (SELECT [ID] AS tmpID14,
					(SELECT CASE
						WHEN ([���_�����������_������] IS NOT NULL) THEN
							([���_�����������_������])
						WHEN ([���_���_�����������_������] IS NOT NULL) THEN
							([���_���_�����������_������])
						ELSE
							(0)
						END AS [Mel_Kathisterisi_Triton]
					) AS [Mel_Kathisterisi_Triton]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp14
				ON tmp14.tmpID14 = [����].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID15,
	--				(SELECT CASE
	--					WHEN ([���_�����������_������] IS NOT NULL) THEN
	--						([���_�����������_������])
	--					ELSE
	--						(0)
	--					END AS [Kat_Kathisterisi_Pelati]
	--				) AS [Kat_Kathisterisi_Pelati]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp15
	--			ON tmp15.tmpID15 = [����].ID
				
	--INNER JOIN (SELECT [ID] AS tmpID16,
	--				(SELECT CASE
	--					WHEN ([���_�����������_���] IS NOT NULL) THEN
	--						([���_�����������_���])
	--					ELSE
	--						(0)
	--					END AS [Kat_Kathisterisi_DEH]
	--				) AS [Kat_Kathisterisi_DEH]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp16
	--			ON tmp16.tmpID16 = [����].ID

	--INNER JOIN (SELECT [ID] AS tmpID17,
	--				(SELECT CASE
	--					WHEN ([���_�����������_������] IS NOT NULL) THEN
	--						([���_�����������_������])
	--					ELSE
	--						(0)
	--					END AS [Kat_Kathisterisi_Triton]
	--				) AS [Kat_Kathisterisi_Triton]
	--			FROM [YLIKA_KOSTOL].[dbo].[����]) tmp17
	--			ON tmp17.tmpID17 = [����].ID
					
	INNER JOIN (SELECT [ID] AS tmpID24, DATEDIFF(day, [����_�������], [����_�����������]) AS [Kathisterisi_AitisisKataxorisis]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp24
				ON tmp24.tmpID24 = [����].ID
				
	INNER JOIN (SELECT [ID] AS tmpID25, DATEDIFF(day, [����_�����������], [����_�������]) AS [Kathisterisi_Meletis]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp25
				ON tmp25.tmpID25 = [����].ID
				
	INNER JOIN (SELECT [ID] AS tmpID22, DATEDIFF(day, [����_�������], [����_����������]) AS [Kathisterisi_Anagelias]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp22
				ON tmp22.tmpID22 = [����].ID

	INNER JOIN (SELECT ID AS tmpID26, SIN(((datepart(dayofyear, [����_�������])) * 360) / 366) AS [DayOfYearSine]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp26
				ON tmp26.tmpID26 = [����].ID
				
	INNER JOIN (SELECT ID AS tmpID27, COS(((datepart(dayofyear, [����_�������])) * 360) / 366) AS [DayOfYearCosine]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp27
				ON tmp27.tmpID27 = [����].ID
				
	INNER JOIN (SELECT ID AS tmpID28, (datepart(year, [����_�������]) * COS(((datepart(dayofyear, [����_�������])) * 360) / 366)) AS [DayOfYearCartesX]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp28
				ON tmp28.tmpID28 = [����].ID

	INNER JOIN (SELECT ID AS tmpID29, (datepart(year, [����_�������]) * SIN(((datepart(dayofyear, [����_�������])) * 360) / 366)) AS [DayOfYearCartesY]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp29
				ON tmp29.tmpID29 = [����].ID

	INNER JOIN (SELECT [ID] AS tmpID30,
		(SELECT CASE
			WHEN ([Label] = 0 AND DATEADD(MONTH,2,[����_�������]) >= '2016-07-06') THEN
				(1)
			ELSE
				(0)
			END AS [MarkedForTest]
		) AS [MarkedForTest]
	FROM [YLIKA_KOSTOL].[dbo].[����]
		INNER JOIN (SELECT [ID] AS tmpID31,
					(SELECT CASE
						WHEN [����_���������] IS NULL THEN
							(0)
						ELSE
							(1)
						END AS [Label]
					) AS [Label]
				FROM [YLIKA_KOSTOL].[dbo].[����]) tmp31
				ON tmp31.tmpID31 = [����].ID) tmp30
	ON tmp30.tmpID30 = [����].ID
															
  WHERE (([���_�������] <> 1 OR [���_�������] IS NULL) AND  --Getting only Clients --1=DEH
		 ([��������] = 0 OR [��������] IS NULL) AND  --Akyrothen <> 0 = false record, ergo non needed
		 [Onoma_Polis] IS NOT NULL AND --216171 --Needed for the Clustering
		 --([Hmerominia] IS NOT NULL) AND --NOT Null so that we can have an accurate Label --The difference between those dates is negligible, so if the one is NULL, the other is used
		 
		 --For the Real-data algorithm, we need the whole thing to pass through Clustering, and this clause will be applied afterwards programmatically
		 --([Label] = 0 AND DATEADD(MONTH,2,[����_�������]) >= '2016-07-06') AND --[Instead of '2016-07-06', it should be GETDATE() on the real HEDNO's server] --Getting the as of yet undecided projects
		 
		 [Xaraktirismos_Ergou] IS NOT NULL AND --The database encompasses a wide range of things but we only care for projects. Those are the 41,D,42,M. The NOT NULL means that that's all the query retrieves
		 [Skopos_Ergou] IS NOT NULL AND --The feature's been engineered so that only rows with a value on this are relevant
		 
		 [������_�������] >= 0 AND --A study cannot have ended before it even began, hence erroneous data (noise) are being cleaned
		 [������_���������_����������] IS NOT NULL AND --Cost is a critical variable, so it must be filled in
		 [������_������_����������] IS NOT NULL AND --Cost is a critical variable, so it must be filled in
		 [������_����������] IS NOT NULL AND --Cost is a critical variable, so it must be filled in
		 [������_�����������_��������] IS NOT NULL AND --Cost is a critical variable, so it must be filled in

		 [������_���������_����������] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects
		 [������_������_����������] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects
		 [������_����������] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects
		 [������_�����������_��������] > 0 AND --A negative cost points to a dismantlement, which are irrelevant projects

		 [MONADA] IS NOT NULL AND
		 [���������] IS NOT NULL AND
		 [������_�������] IS NOT NULL AND
		 [���������_�������] IS NOT NULL AND
		 [������_�����] IS NOT NULL AND
		 [������_��] IS NOT NULL AND

		 [����_�������] IS NOT NULL AND
		 [����_�����������] IS NOT NULL AND
		 [����_����������] IS NOT NULL AND
		 [����_�������] IS NOT NULL --AND

		 --[SAP_�����_������] IS NOT NULL AND
		 --[SAP_�����_���������] IS NOT NULL --AND
		 )