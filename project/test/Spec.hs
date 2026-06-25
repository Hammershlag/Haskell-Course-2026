module Main (main) where

import Test.Hspec
import qualified Data.Map as Map
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses a contract with state and transactions" $ do
      let src = "contract TestCoin {\n\
                \  state {\n\
                \    balances: map<address, int> = empty;\n\
                \    owner:    address          = sender;\n\
                \  }\n\
                \  transaction mint(to: address, amount: int) {\n\
                \    require sender == owner;\n\
                \    balances[to] := balances[to] + amount;\n\
                \  }\n\
                \  transaction transfer(to: address, amount: int) {\n\
                \    require balances[sender] >= amount;\n\
                \    balances[sender] := balances[sender] - amount;\n\
                \    balances[to]     := balances[to] + amount;\n\
                \  }\n\
                \}"
      case parseContract "test" src of
        Left err -> expectationFailure err
        Right c -> do
          contractName c `shouldBe` "TestCoin"
          length (contractStateVars c) `shouldBe` 2
          length (contractTransactions c) `shouldBe` 2

    it "ignores comments" $ do
      let src = "// comment\n\
                \contract TestCoin {\n\
                \  /* block comment */\n\
                \  state {}\n\
                \}"
      case parseContract "test" src of
        Left err -> expectationFailure err
        Right c -> contractName c `shouldBe` "TestCoin"

  describe "VM" $ do
    let parsed = parseContract "test"
          "contract TestCoin {\n\
          \  state {\n\
          \    balances: map<address, int> = empty;\n\
          \    owner:    address          = sender;\n\
          \  }\n\
          \  transaction mint(to: address, amount: int) {\n\
          \    require sender == owner;\n\
          \    balances[to] := balances[to] + amount;\n\
          \  }\n\
          \  transaction transfer(to: address, amount: int) {\n\
          \    require balances[sender] >= amount;\n\
          \    balances[sender] := balances[sender] - amount;\n\
          \    balances[to]     := balances[to] + amount;\n\
          \  }\n\
          \}"
    let getContract = case parsed of
          Right c -> c
          Left err -> error err

    it "initializes state correctly" $ do
      let contract = getContract
      case initContractState contract "0xowner" of
        Left err -> expectationFailure err
        Right st -> do
          Map.lookup "owner" st `shouldBe` Just (VAddress "0xowner")
          Map.lookup "balances" st `shouldBe` Just (VMap Map.empty)

    it "validates transaction signatures and arguments" $ do
      let contract = getContract
      let txInvalidName = Transaction "mint2" [VAddress "0xalice", VInt 100] "0xowner"
      validateTransaction contract txInvalidName `shouldSatisfy` (\r -> case r of
        Left _ -> True
        Right _ -> False)

      let txInvalidCount = Transaction "mint" [VAddress "0xalice"] "0xowner"
      validateTransaction contract txInvalidCount `shouldSatisfy` (\r -> case r of
        Left _ -> True
        Right _ -> False)

      let txInvalidType = Transaction "mint" [VAddress "0xalice", VBool True] "0xowner"
      validateTransaction contract txInvalidType `shouldSatisfy` (\r -> case r of
        Left _ -> True
        Right _ -> False)

    it "executes valid transaction and updates state" $ do
      let contract = getContract
      let state0 = case initContractState contract "0xowner" of
            Right st -> st
            Left err -> error err
      let tx = Transaction "mint" [VAddress "0xalice", VInt 100] "0xowner"
      case executeTransaction contract tx state0 of
        Left err -> expectationFailure err
        Right state1 -> do
          let balancesVal = Map.lookup "balances" state1
          case balancesVal of
            Just (VMap m) -> Map.lookup (VAddress "0xalice") m `shouldBe` Just (VInt 100)
            _ -> expectationFailure "balances is not a map"

    it "reverts state on require failure" $ do
      let contract = getContract
      let state0 = case initContractState contract "0xowner" of
            Right st -> st
            Left err -> error err
      let tx = Transaction "mint" [VAddress "0xalice", VInt 100] "0xalice"
      executeTransaction contract tx state0 `shouldSatisfy` (\r -> case r of
        Left _ -> True
        Right _ -> False)

  describe "Ledger" $ do
    let contract = case parseContract "test"
          "contract TestCoin {\n\
          \  state {\n\
          \    balances: map<address, int> = empty;\n\
          \    owner:    address          = sender;\n\
          \  }\n\
          \  transaction mint(to: address, amount: int) {\n\
          \    require sender == owner;\n\
          \    balances[to] := balances[to] + amount;\n\
          \  }\n\
          \  transaction transfer(to: address, amount: int) {\n\
          \    require balances[sender] >= amount;\n\
          \    balances[sender] := balances[sender] - amount;\n\
          \    balances[to]     := balances[to] + amount;\n\
          \  }\n\
          \}" of
            Right c -> c
            Left err -> error err

    it "manages valid blocks and evaluates ledger state chronologically" $ do
      let ledger0 = emptyLedger
      let block1 = Block 1 (Just 0)
            [ Transaction "mint" [VAddress "0xalice", VInt 100] "0xowner"
            , Transaction "transfer" [VAddress "0xbob", VInt 150] "0xalice"
            , Transaction "transfer" [VAddress "0xbob", VInt 40] "0xalice"
            ]
      case addBlock ledger0 block1 of
        Left err -> expectationFailure err
        Right ledger1 -> do
          verifyLedgerIntegrity ledger1 `shouldBe` Right ()
          case evaluateLedgerState contract "0xowner" ledger1 of
            Left err -> expectationFailure err
            Right finalState -> do
              let balancesVal = Map.lookup "balances" finalState
              case balancesVal of
                Just (VMap m) -> do
                  Map.lookup (VAddress "0xalice") m `shouldBe` Just (VInt 60)
                  Map.lookup (VAddress "0xbob") m `shouldBe` Just (VInt 40)
                _ -> expectationFailure "balances is not a map"

    it "rejects blocks with invalid parent references" $ do
      let ledger0 = emptyLedger
      let badBlock = Block 1 (Just 999) []
      addBlock ledger0 badBlock `shouldSatisfy` (\r -> case r of
        Left _ -> True
        Right _ -> False)

    it "rejects blocks with duplicate IDs" $ do
      let ledger0 = emptyLedger
      let duplicateBlock = Block 0 (Just 0) []
      addBlock ledger0 duplicateBlock `shouldSatisfy` (\r -> case r of
        Left _ -> True
        Right _ -> False)
