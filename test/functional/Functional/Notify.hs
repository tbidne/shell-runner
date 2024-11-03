-- | Functional tests for notifications
module Functional.Notify (specs) where

import DBus.Notify (UrgencyLevel (Normal))
import Functional.Prelude
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutSeconds),
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

specs :: TestTree
specs = testGroup "Notify" notifyTests

notifyTests :: List TestTree
notifyTests =
  [ notifySystem,
    notifyActionCommand,
    notifyActionAll,
    notifyTimeout5
  ]

-- "Missing tests" i.e.
--
--   - notifySystemDBus
--   - notifyActionFinal
--   - notifyActionTimeoutNever
--
-- are part of the Examples test module

notifySystem :: TestTree
notifySystem = testCase ("Runs --notify-system " <> notifySystemArg) $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "command",
          "--notify-system",
          notifySystemArg,
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyActionCommand :: TestTree
notifyActionCommand = testCase "Runs --notify-action command" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "command",
          "--notify-system",
          notifySystemArg,
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyActionAll :: TestTree
notifyActionAll = testCase "Runs --notify-action all" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "all",
          "--notify-system",
          notifySystemArg,
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyTimeout5 :: TestTree
notifyTimeout5 = testCase "Runs --notify-timeout 5" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "all",
          "--notify-system",
          notifySystemArg,
          "--notify-timeout",
          "5",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 5
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 5
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 5
          }
      ]
