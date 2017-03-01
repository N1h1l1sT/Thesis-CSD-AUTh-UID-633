/******************
**** Mel Ylika ****
******************/
USE YLIKA_KOSTOL
GO

CREATE VIEW [dbo].[v1Mel_Ylika]
AS
SELECT [id] as [ID_Ylika]
		,[�����������] as [Promitheftis]
		,[������������] as [Epimetrisimo]
		--,UPPER(RTRIM([������ ��������])) as [Monada_Metrisis] --inconsequential
		--,UPPER(RTRIM([���������])) as [Enimerosi] --inconsequential
		,[���� �] as [Timi_A] --Item's Price
		--,[���� �] as [Timi_B] --B prices are treated as A prices for new projects
		--,[���� �] as [Timi_C] --Isn't used for new projects
		--,[���� �] as [Timi_X] --Isn't used for new projects
FROM [YLIKA_KOSTOL].[dbo].[���_�����]
WHERE ([���_�����].[���� �] IS NOT NULL) AND --We only case about items that cost something
		([���_�����].[���� �] > 0)			 --Apart from having a cost, the cost must be above a threshold
