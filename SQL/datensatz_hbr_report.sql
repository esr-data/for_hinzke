WITH
datenbasis AS (
  SELECT *
    FROM daten
  WHERE daten.id IN (
    SELECT reihe_id as id
    FROM tag_link
    WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'daten') AND
    tag_id = (SELECT id FROM tag WHERE bez = 'hbr_daten')
  )
)
SELECT
  datenbasis.id AS id,
  variable.beschr AS Variable,
  date_part('year', datenbasis.zeit_ende) AS Jahr,
  datenbasis.zeit_start AS Zeit,
  COALESCE(land.beschr, bundesland.beschr) AS Region,
  hochschulart.beschr AS g_Hochschultyp,
  geschlecht.beschr AS g_Geschlecht,
  datenbasis.wert AS Wert,
  wert_einheit.beschr AS Einheit,
  quelle.beschr AS Quelle
FROM datenbasis
LEFT JOIN (
  SELECT *
    FROM daten_reichweite
  LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
  WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'hochschulart')
) hochschulart ON datenbasis.id = hochschulart.daten_id
LEFT JOIN (
  SELECT *
    FROM daten_reichweite
  LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
  WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'geschlecht')
) geschlecht ON datenbasis.id = geschlecht.daten_id
LEFT JOIN (
  SELECT *
    FROM daten_reichweite
  LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
  WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'bundesland')
) bundesland ON datenbasis.id = bundesland.daten_id
LEFT JOIN (
  SELECT *
    FROM daten_reichweite
  LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
  WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'land')
) land ON datenbasis.id = land.daten_id
LEFT JOIN (
  SELECT daten_quelle.daten_id as daten_id, string_agg(beschr, ', ' ORDER BY beschr DESC) AS beschr
    FROM daten_quelle
  LEFT JOIN quelle ON daten_quelle.quelle_id = quelle.id
  GROUP BY daten_id
) quelle ON datenbasis.id = quelle.daten_id
LEFT JOIN wert_einheit ON datenbasis.wert_einheit_id = wert_einheit.id
LEFT JOIN variable ON datenbasis.variable_id = variable.id
