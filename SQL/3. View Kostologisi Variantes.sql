/*****************************
**** Mel Kostol Variantes ****
*****************************/
USE YLIKA_KOSTOL
GO

CREATE VIEW [dbo].[v3Mel_Kostol_Variantes]
AS
SELECT [ID] as [ID_Kostol_Variantes]
      ,[Κωδικός Μελέτης] as [Kodikos_Meletis_FromKostolVar]
      ,[Κωδικός Βαριάντας] as [Kodikos_Variantas_FromKostolVar]
      --,[Ονομασία]
      ,[Τοποθέτηση] as [Topothetisi_FromKostolVar]
      --,[Αποξήλωση] as [Apoksilosi_FromKostolVar] --inconsequential
      ,[Μετατόπιση] as [Metatopisi]
      --,[ΕΙΔΟΣ_ΜΣ] --inconsequential
      --,[ΙΣΧΥΣ_ΜΣ] --inconsequential
      --,[Α_Α_ΔΙΚΤΥΟΥ] --inconsequential
      ,[Αλλαγή] as [Allagi]
  FROM [YLIKA_KOSTOL].[dbo].[ΜΕΛ_ΚΟΣΤΟΛΟΓΗΣΗ_ΒΑΡΙΑΝΤΕΣ]