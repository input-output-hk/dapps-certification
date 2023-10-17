  -- smaller version of profileSummary.sql without subscriptions
  SELECT p.*
       , COALESCE(r.profileId, -1) AS profileId
       , COALESCE(r.role, 0) AS role
       , d.*
  FROM profile AS p

  LEFT JOIN (
      SELECT profileId, MAX(role) AS role
      FROM profile_role
      GROUP BY profileId
  ) AS r
  ON p.profileId = r.profileId

  LEFT JOIN ( select * from dapp ) AS d
  ON p.profileId = d.dappId
