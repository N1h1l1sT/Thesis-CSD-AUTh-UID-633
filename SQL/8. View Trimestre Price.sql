/******************
**** Trimestre ****
******************/
USE YLIKA_KOSTOL
GO

CREATE VIEW [dbo].[v8Price_Per_Trimestre]
--ALTER VIEW [dbo].[v8Price_Per_Trimestre]
AS
SELECT [TimeSeriesDate], sum([PriceSum]) as TrimestrePriceSum 
       FROM [YLIKA_KOSTOL].[dbo].[v7Aggr_Erga]
	   Group by [v7Aggr_Erga].[TimeSeriesDate]