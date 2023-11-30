SELECT
 view_tag_baum_lesbar.id,
 view_tag_baum_lesbar.text,
 tag.beschr
FROM view_tag_baum_lesbar
LEFT JOIN
 tag ON view_tag_baum_lesbar.id = tag.id
WHERE
 view_tag_baum_lesbar.id IN
 (SELECT tag_id FROM tag_link WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable'));
