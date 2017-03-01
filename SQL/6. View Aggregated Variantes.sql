/*****************************
**** Aggregated Variantes ****
*****************************/
USE YLIKA_KOSTOL
GO

--CREATE VIEW [dbo].[v6Mel_Kostol_Aggr_Variantes]
ALTER VIEW [dbo].[v6Mel_Kostol_Aggr_Variantes]
AS
SELECT v3Mel_Kostol_Variantes.ID_Kostol_Variantes as Kostol_VariantID
	   ,Allagi
	   ,Kodikos_Meletis_FromKostolVar
	   ,Metatopisi
	   ,Topothetisi_FromKostolVar
	   ,PriceSum as ItemPriceSum
  FROM YLIKA_KOSTOL.dbo.v3Mel_Kostol_Variantes
  INNER JOIN (SELECT tmp.Kodikos_VariantasMel_FromAnalysYlika, SUM(tmp.Price) as PriceSum
			  FROM (SELECT Kodikos_VariantasMel_FromAnalysYlika, Timi_A as Price
					FROM [YLIKA_KOSTOL].[dbo].v2Mel_Kostol_Analys_Ylika
					INNER JOIN [YLIKA_KOSTOL].[dbo].v1Mel_Ylika ON v2Mel_Kostol_Analys_Ylika.Kodikos_Ylikou_FromAnalysYlika = v1Mel_Ylika.ID_Ylika) as tmp
			  GROUP BY tmp.Kodikos_VariantasMel_FromAnalysYlika
			  ) tmp
  ON v3Mel_Kostol_Variantes.ID_Kostol_Variantes = tmp.Kodikos_VariantasMel_FromAnalysYlika

/*
	SELECT count( Timi_A)
	FROM [YLIKA_KOSTOL].[dbo].[vMel_Kostol_Analys_Ylika]
	INNER JOIN [YLIKA_KOSTOL].[dbo].vMel_Ylika ON [vMel_Kostol_Analys_Ylika].Kodikos_Ylikou = vMel_Ylika.ID
Gives 17,103,048 results

SELECT COUNT([vMel_Kostol_Aggr_Variantes].Kostol_VariantID)
FROM [YLIKA_KOSTOL].[dbo].[vMel_Kostol_Aggr_Variantes]
    2,130 valid kodikoi variantas (vMel_Kostol_Variantes.ID) in [vMel_Kostol_Aggr_Variantes]

SELECT COUNT(DISTINCT [vMel_Kostol_Variantes].ID)
FROM [YLIKA_KOSTOL].[dbo].[vMel_Kostol_Variantes]
2,533,079 Distinct kodikoi variantas (vMel_Kostol_Variantes.ID) in vMel_Kostol_Variantes

SELECT COUNT(DISTINCT [vMel_Kostol_Variantes].Kodikos_Meletis)
FROM [YLIKA_KOSTOL].[dbo].[vMel_Kostol_Variantes]
  292,026 Distinct Kodikoi meletis (erga) in vMel_Kostol_Variantes

SELECT COUNT(DISTINCT [vMel_Kostol_Analys_Ylika].Kodikos_Bariantas)
FROM [YLIKA_KOSTOL].[dbo].[vMel_Kostol_Analys_Ylika]
    2,563 Distinct kodikoi variantas (vMel_Kostol_Variantes.ID) in [vMel_Kostol_Analys_Ylika] !!!!

*/

