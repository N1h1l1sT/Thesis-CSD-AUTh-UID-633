/******************
**** Mel Ylika ****
******************/
USE YLIKA_KOSTOL
GO

CREATE VIEW [dbo].[v1Mel_Ylika]
AS
SELECT [id] as [ID_Ylika]
		,[Προμηθευτής] as [Promitheftis]
		,[Επιμετρήσιμο] as [Epimetrisimo]
		--,UPPER(RTRIM([Μονάδα Μέτρησης])) as [Monada_Metrisis] --inconsequential
		--,UPPER(RTRIM([Ενημέρωση])) as [Enimerosi] --inconsequential
		,[Τιμή Α] as [Timi_A] --Item's Price
		--,[Τιμή Β] as [Timi_B] --B prices are treated as A prices for new projects
		--,[Τιμή Γ] as [Timi_C] --Isn't used for new projects
		--,[Τιμή Χ] as [Timi_X] --Isn't used for new projects
FROM [YLIKA_KOSTOL].[dbo].[ΜΕΛ_ΥΛΙΚΑ]
WHERE ([ΜΕΛ_ΥΛΙΚΑ].[Τιμή Α] IS NOT NULL) AND --We only case about items that cost something
		([ΜΕΛ_ΥΛΙΚΑ].[Τιμή Α] > 0)			 --Apart from having a cost, the cost must be above a threshold
