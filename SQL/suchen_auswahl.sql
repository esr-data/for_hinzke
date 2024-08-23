WITH fundament AS (
	SELECT DISTINCT daten_id, reihe_id, tabelle_id, 1 AS n
	FROM view_daten_link vdl
	WHERE vdl.daten_id IN (SELECT daten_id FROM view_daten_link WHERE %s) AND
	      vdl.tabelle_id IN (%s)
),
datenpunkte AS (SELECT COUNT(DISTINCT daten_id) as n FROM fundament),
fundament_link AS (SELECT CONCAT(reihe_id, '-', tabelle_id) as link_id, n FROM fundament),
fundament_link_agg AS (SELECT link_id, SUM(n) as n FROM fundament_link GROUP BY link_id),
paket AS (
	SELECT
		fundament.*,
		CONCAT (reichweite.beschr, reichweite_typ.beschr, variable.beschr, wert_einheit.beschr) as beschr,
		reichweite.reichweite_typ_id,
		reichweite_typ.reichweite_klasse_id = (SELECT id FROM reichweite_klasse WHERE bez_lang = 'region') as is_region,
		CONCAT(reihe_id, '-', tabelle_id) AS link_id
	FROM fundament
	LEFT JOIN reichweite ON fundament.reihe_id = reichweite.id AND fundament.tabelle_id = %s
	LEFT JOIN wert_einheit ON fundament.reihe_id = wert_einheit.id AND fundament.tabelle_id = %s
	LEFT JOIN reichweite_typ ON fundament.reihe_id = reichweite_typ.id AND fundament.tabelle_id = %s
	LEFT JOIN variable ON fundament.reihe_id = variable.id AND fundament.tabelle_id = %s
)
SELECT DISTINCT
  CONCAT(paket.reihe_id, '-', paket.tabelle_id) AS id,
	CASE WHEN paket.is_region IS NULL
	     THEN paket.reihe_id
	     ELSE (
	      CASE WHEN paket.is_region
	           THEN CONCAT('a', paket.reihe_id)
	           ELSE CONCAT('b', paket.reihe_id)
	      END
	     )
	     END AS gruppe_id,
	paket.tabelle_id,
	paket.reichweite_typ_id,
	paket.beschr,
	paket.is_region,
	ROUND(agg.n / (SELECT n FROM datenpunkte) * 100, 2) as prozent
FROM paket
LEFT JOIN fundament_link_agg agg ON paket.link_id = agg.link_id
