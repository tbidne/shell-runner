module Functional.Examples.Notify (tests) where

import DBus.Notify (UrgencyLevel (Normal))
import Functional.Prelude
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Notify.Effect
  ( ShrunNote
      ( MkShrunNote,
        body,
        summary,
        timeout,
        urgency
      ),
  )

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "Notify"
    [ notifyActionFinal,
      notifyTimeoutNever
    ]

-- NOTE: There is no DBus test because that requires creating a real DBus
-- connection, as we are running in IO.

notifyActionFinal :: TestTree
notifyActionFinal = testCase "Runs --notify-action final" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          notifySystemArg,
          "--notify-action",
          "final",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyTimeoutNever :: TestTree
notifyTimeoutNever = testCase "Runs --notify-timeout never" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          notifySystemArg,
          "--notify-action",
          "all",
          "--notify-timeout",
          "never",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutNever
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutNever
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutNever
          }
      ]
