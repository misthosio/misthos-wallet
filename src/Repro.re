type dummyInput = {
  txId: string,
  txOutputN: int,
};

module TxDummyInputCmp =
  Belt.Id.MakeComparableU(
    {
      type t = dummyInput;
      let cmp =
        (.
          {txId: id1, txOutputN: out1}: dummyInput,
          {txId: id2, txOutputN: out2}: dummyInput,
        ) =>
          compare(
            id1 ++ ":" ++ string_of_int(out1),
            id2 ++ ":" ++ string_of_int(out2),
          );
    },
  );

type dummySet = Belt.Set.t(TxDummyInputCmp.t, TxDummyInputCmp.identity);

let dummySet = () => Belt.Set.make(~id=(module TxDummyInputCmp));

let dummyBefore =
  dummySet()
  |. Belt.Set.mergeMany([|
       {
         txId: "124ca66f71479a5bec64727e04d43d5d3bf390fa1fa1a4d880c1ef07125bcef8",
         txOutputN: 1,
       },
       {
         txId: "1fd8d28743b218ee09297a923f328574e880955a45f5c35b4a2fba72b29e7439",
         txOutputN: 1,
       },
       {
         txId: "2674d7197f331f08ddc2c7b9c68618d935ec58ec314da018e8ce997020f444c0",
         txOutputN: 1,
       },
       {
         txId: "26b6ddad37f9edc4ceebb8f1384fea5f62fdad3caf5f3a6bfc6e8b6bc461aea9",
         txOutputN: 1,
       },
       {
         txId: "514ec6088ef79a9c56b1530b6d0e1a47fc5e61ab74993861e315d1430de2c407",
         txOutputN: 0,
       },
       {
         txId: "773cbcba2d34cf2504b0ec581656fd0da30261df98629731dd78c3d816d5ec24",
         txOutputN: 1,
       },
       {
         txId: "7ac21a22271c24d930f56023f0a7d6b8ecafd809726a73f8fbdfd0b749cde3b8",
         txOutputN: 1,
       },
       {
         txId: "7e12e0eec5a379cea826734be1248050229903c1267333f743e1d9db9508905a",
         txOutputN: 1,
       },
       {
         txId: "7e25c3b8399b3ff24c1ba48e0c6e45e19cc8b43d5128616ad689220187194b14",
         txOutputN: 1,
       },
       {
         txId: "7ea4150658f6830482f6cef7be7441474fa7faa8fc99b7c86ff1a62a33543819",
         txOutputN: 1,
       },
       {
         txId: "91872071cc235570efeb149d09c161c8f378ddd3e7f344363ac8fe0ad2c5d30a",
         txOutputN: 1,
       },
       {
         txId: "aa4d81f03aaeb5980ea273cf50238f688bb6448943ff6ce875e98adb0a4f75c8",
         txOutputN: 1,
       },
       {
         txId: "b0478fed46339ffd2d0d36b0355d782be269b0452f452d7532b8f6e1dfa8e06b",
         txOutputN: 1,
       },
       {
         txId: "b54f5481e48af0cf20700aceccb03c634b8e884e56f4f7def798014a1da07abc",
         txOutputN: 1,
       },
       {
         txId: "b724db70e7822af0d5e1f60676b5eead33d7a38a9b1ab5659e15120573c0c894",
         txOutputN: 1,
       },
       {
         txId: "dab68eb6dfac5258cd6274c558d75b112bf324cebce0232884215c830c53b893",
         txOutputN: 1,
       },
       {
         txId: "e0c750a6bf90fe2036d10377852330b704026ce782d89bd17b18337f2de945a1",
         txOutputN: 1,
       },
       {
         txId: "eb782bdb77cb3ceb1efe2d7179104bb903a3c1bb83202e207e00438519aacb8b",
         txOutputN: 1,
       },
     |]);

let keepTx = ({txId}) =>
  txId != "514ec6088ef79a9c56b1530b6d0e1a47fc5e61ab74993861e315d1430de2c407";

let dummyAfter = dummyBefore |. Belt.Set.keep(keepTx);
