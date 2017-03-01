/*******************
**** All In One ****
*******************/
USE YLIKA_KOSTOL
GO

--CREATE VIEW [dbo].[v5AIO]
ALTER VIEW [dbo].[v5AIO]
AS
SELECT *
FROM [YLIKA_KOSTOL].[dbo].[v4Erga]
INNER JOIN [YLIKA_KOSTOL].[dbo].[v3Mel_Kostol_Variantes]
ON [YLIKA_KOSTOL].[dbo].[v4Erga].ID_Erga = [YLIKA_KOSTOL].[dbo].[v3Mel_Kostol_Variantes].Kodikos_Meletis_FromKostolVar
INNER JOIN [YLIKA_KOSTOL].[dbo].[v2Mel_Kostol_Analys_Ylika]
ON [YLIKA_KOSTOL].[dbo].[v3Mel_Kostol_Variantes].ID_Kostol_Variantes = [YLIKA_KOSTOL].[dbo].[v2Mel_Kostol_Analys_Ylika].Kodikos_VariantasMel_FromAnalysYlika
INNER JOIN [YLIKA_KOSTOL].[dbo].v1Mel_Ylika
ON [YLIKA_KOSTOL].[dbo].[v2Mel_Kostol_Analys_Ylika].Kodikos_Ylikou_FromAnalysYlika = [YLIKA_KOSTOL].[dbo].v1Mel_Ylika.ID_Ylika


/*
CREATE VIEW [dbo].[vAIO]
AS
SELECT [Promitheftis]
      ,[Epimetrisimo]
      ,UPPER(RTRIM([Monada_Metrisis])) AS [Monada_Metrisis]
      ,UPPER(RTRIM([Enimerosi])) AS [Enimerosi]
      ,[Timi_A]
	  ,[Topothetisi]
      ,[Timi_Topothetisis]
      ,[Aksia_Topothetisis]
      ,[Apoksilosi]
      ,[Timi_Apoksilosis]
	  ,[Aksia_Apoksilosis]
	  ,[Topothetisi_Var]
      ,[Apoksilosi_Var]
      ,[Metatopisi]
      ,[Allagi]
FROM [YLIKA_KOSTOL].[dbo].Erga
INNER JOIN [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Variantes]
ON [YLIKA_KOSTOL].[dbo].Erga.ID = [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Variantes].Kodikos_Meletis
INNER JOIN [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika]
ON [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Variantes].ID = [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika].Kodikos_Bariantas
INNER JOIN [YLIKA_KOSTOL].[dbo].[Mel_Ylika]
ON [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika].Kodikos_Ylikou = [YLIKA_KOSTOL].[dbo].[Mel_Ylika].id

FROM [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Variantes]
INNER JOIN [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika]
ON [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Variantes].ID = [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika].Kodikos_Bariantas
INNER JOIN [YLIKA_KOSTOL].[dbo].[Mel_Ylika]
ON [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika].Kodikos_Ylikou = [YLIKA_KOSTOL].[dbo].[Mel_Ylika].id
*/
/*
FROM [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika]
INNER JOIN [YLIKA_KOSTOL].[dbo].[Mel_Ylika]
ON [YLIKA_KOSTOL].[dbo].[Mel_Kostol_Analys_Ylika].Kodikos_Ylikou = [YLIKA_KOSTOL].[dbo].[Mel_Ylika].id
*/