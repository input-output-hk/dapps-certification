-- Create a CTE to rank subscriptions for each profile
-- Prioritizing Status in order 1, 0, and then anything greater than 1.
-- Among those with the same status, the most recent subscription is prioritized.
WITH RankedSubscriptions AS (
    SELECT
      *,
      ROW_NUMBER() OVER (
          PARTITION BY ProfileId
          ORDER BY
              CASE
                  WHEN Status = 1 THEN 0
                  WHEN Status = 2 THEN 1
                  ELSE 2
              END,
              EndDate DESC
      ) AS rn
    FROM
      subscription
    ),
    -- Create a CTE to calculate the number of runs for each profile
    runs AS (
        SELECT
            profileId,

            COUNT(*) as total_runs,
            SUM(CASE WHEN r.runStatus = 'Succeeded' THEN 1 ELSE 0 END) as successful_runs,
            SUM(CASE WHEN r.runStatus = 'Queued' THEN 1 ELSE 0 END) as queued_runs,
            SUM(CASE WHEN r.runStatus = 'Failed' THEN 1 ELSE 0 END) as failed_runs,
            SUM(CASE WHEN r.runStatus = 'ReadyForCertification' THEN 1 ELSE 0 END) as ready_for_certification_runs,
            SUM(CASE WHEN r.runStatus = 'Certified' THEN 1 ELSE 0 END) as certified_runs,
            SUM(CASE WHEN r.runStatus = 'Aborted' THEN 1 ELSE 0 END) as aborted_runs
        FROM run r
        GROUP BY profileId
    )
  -- Main query to join profiles with roles, dapps, and their ranked subscriptions
  SELECT
    -- Selecting all columns from profiles
    p.*,

    -- If no role is found for a profile, default values are assigned
    COALESCE(r.profileId, -1) AS profileId,
    COALESCE(r.role, 0) AS role,

    -- Specifying columns for dapp
    COALESCE(d.dappId,-1) AS dappId,
    COALESCE(d.dappName,'' ) AS dappName,
    COALESCE(d.dappOwner,'' ) AS dappOwner,
    COALESCE(d.dappRepo,'' ) AS dappRepo,
    COALESCE(d.dappVersion,'' ) AS dappVersion,
    d.dappGitHubToken,
    d.dappSubject,

    -- Default values for runs
    COALESCE(runs.profileId, -1) AS runsProfileId,
    COALESCE(runs.total_runs, 0) AS runsTotal,
    COALESCE(runs.successful_runs, 0) AS runsSuccessful,
    COALESCE(runs.queued_runs, 0) AS runsQueued,
    COALESCE(runs.failed_runs, 0) AS runsFailed,
    COALESCE(runs.ready_for_certification_runs, 0) AS runsReadyForCertification,
    COALESCE(runs.certified_runs, 0) AS runsCertified,
    COALESCE(runs.aborted_runs, 0) AS runsAborted,


    -- Selecting specific columns from the ranked subscriptions
    rs.Id as subId ,
    rs.Name as subName ,
    rs.Type as subTierType ,
    rs.Price as subPrice ,
    rs.AdaUsdPrice as subAdaUsdPrice ,
    rs.StartDate as subStartDate ,
    rs.EndDate as subEndDate ,
    rs.Status as subStatus

  FROM
      profile AS p

  -- Joining profiles with roles to determine the maximum role for each profile
  LEFT JOIN (
      SELECT
          profileId,
          MAX(role) AS role
      FROM
          profile_role
      GROUP BY
          profileId
  ) AS r
  ON
      p.profileId = r.profileId

  -- Joining profiles with dapps
  LEFT JOIN
      dapp AS d
  ON
      p.profileId = d.dappId

  -- Joining profiles with their highest-priority subscription using the earlier CTE
  LEFT JOIN
      RankedSubscriptions rs
  ON
      p.profileId = rs.ProfileId AND rs.rn = 1

  -- Joining profiles with their runs
  LEFT JOIN runs ON p.profileId = runs.profileId

