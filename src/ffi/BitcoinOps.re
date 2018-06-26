type t = Node.buffer;

[@bs.module "bitcoin-ops"] external op_FALSE : t = "OP_FALSE";
[@bs.module "bitcoin-ops"] external op_0 : t = "OP_0";
[@bs.module "bitcoin-ops"] external op_PUSHDATA1 : t = "OP_PUSHDATA1";
[@bs.module "bitcoin-ops"] external op_PUSHDATA2 : t = "OP_PUSHDATA2";
[@bs.module "bitcoin-ops"] external op_PUSHDATA4 : t = "OP_PUSHDATA4";
[@bs.module "bitcoin-ops"] external op_1NEGATE : t = "OP_1NEGATE";
[@bs.module "bitcoin-ops"] external op_RESERVED : t = "OP_RESERVED";
[@bs.module "bitcoin-ops"] external op_TRUE : t = "OP_TRUE";
[@bs.module "bitcoin-ops"] external op_1 : t = "OP_1";
[@bs.module "bitcoin-ops"] external op_2 : t = "OP_2";
[@bs.module "bitcoin-ops"] external op_3 : t = "OP_3";
[@bs.module "bitcoin-ops"] external op_4 : t = "OP_4";
[@bs.module "bitcoin-ops"] external op_5 : t = "OP_5";
[@bs.module "bitcoin-ops"] external op_6 : t = "OP_6";
[@bs.module "bitcoin-ops"] external op_7 : t = "OP_7";
[@bs.module "bitcoin-ops"] external op_8 : t = "OP_8";
[@bs.module "bitcoin-ops"] external op_9 : t = "OP_9";
[@bs.module "bitcoin-ops"] external op_10 : t = "OP_10";
[@bs.module "bitcoin-ops"] external op_11 : t = "OP_11";
[@bs.module "bitcoin-ops"] external op_12 : t = "OP_12";
[@bs.module "bitcoin-ops"] external op_13 : t = "OP_13";
[@bs.module "bitcoin-ops"] external op_14 : t = "OP_14";
[@bs.module "bitcoin-ops"] external op_15 : t = "OP_15";
[@bs.module "bitcoin-ops"] external op_16 : t = "OP_16";

[@bs.module "bitcoin-ops"] external op_NOP : t = "OP_NOP";
[@bs.module "bitcoin-ops"] external op_VER : t = "OP_VER";
[@bs.module "bitcoin-ops"] external op_IF : t = "OP_IF";
[@bs.module "bitcoin-ops"] external op_NOTIF : t = "OP_NOTIF";
[@bs.module "bitcoin-ops"] external op_VERIF : t = "OP_VERIF";
[@bs.module "bitcoin-ops"] external op_VERNOTIF : t = "OP_VERNOTIF";
[@bs.module "bitcoin-ops"] external op_ELSE : t = "OP_ELSE";
[@bs.module "bitcoin-ops"] external op_ENDIF : t = "OP_ENDIF";
[@bs.module "bitcoin-ops"] external op_VERIFY : t = "OP_VERIFY";
[@bs.module "bitcoin-ops"] external op_RETURN : t = "OP_RETURN";

[@bs.module "bitcoin-ops"] external op_TOALTSTACK : t = "OP_TOALTSTACK";
[@bs.module "bitcoin-ops"] external op_FROMALTSTACK : t = "OP_FROMALTSTACK";
[@bs.module "bitcoin-ops"] external op_2DROP : t = "OP_2DROP";
[@bs.module "bitcoin-ops"] external op_2DUP : t = "OP_2DUP";
[@bs.module "bitcoin-ops"] external op_3DUP : t = "OP_3DUP";
[@bs.module "bitcoin-ops"] external op_2OVER : t = "OP_2OVER";
[@bs.module "bitcoin-ops"] external op_2ROT : t = "OP_2ROT";
[@bs.module "bitcoin-ops"] external op_2SWAP : t = "OP_2SWAP";
[@bs.module "bitcoin-ops"] external op_IFDUP : t = "OP_IFDUP";
[@bs.module "bitcoin-ops"] external op_DEPTH : t = "OP_DEPTH";
[@bs.module "bitcoin-ops"] external op_DROP : t = "OP_DROP";
[@bs.module "bitcoin-ops"] external op_DUP : t = "OP_DUP";
[@bs.module "bitcoin-ops"] external op_NIP : t = "OP_NIP";
[@bs.module "bitcoin-ops"] external op_OVER : t = "OP_OVER";
[@bs.module "bitcoin-ops"] external op_PICK : t = "OP_PICK";
[@bs.module "bitcoin-ops"] external op_ROLL : t = "OP_ROLL";
[@bs.module "bitcoin-ops"] external op_ROT : t = "OP_ROT";
[@bs.module "bitcoin-ops"] external op_SWAP : t = "OP_SWAP";
[@bs.module "bitcoin-ops"] external op_TUCK : t = "OP_TUCK";

[@bs.module "bitcoin-ops"] external op_CAT : t = "OP_CAT";
[@bs.module "bitcoin-ops"] external op_SUBSTR : t = "OP_SUBSTR";
[@bs.module "bitcoin-ops"] external op_LEFT : t = "OP_LEFT";
[@bs.module "bitcoin-ops"] external op_RIGHT : t = "OP_RIGHT";
[@bs.module "bitcoin-ops"] external op_SIZE : t = "OP_SIZE";

[@bs.module "bitcoin-ops"] external op_INVERT : t = "OP_INVERT";
[@bs.module "bitcoin-ops"] external op_AND : t = "OP_AND";
[@bs.module "bitcoin-ops"] external op_OR : t = "OP_OR";
[@bs.module "bitcoin-ops"] external op_XOR : t = "OP_XOR";
[@bs.module "bitcoin-ops"] external op_EQUAL : t = "OP_EQUAL";
[@bs.module "bitcoin-ops"] external op_EQUALVERIFY : t = "OP_EQUALVERIFY";
[@bs.module "bitcoin-ops"] external op_RESERVED1 : t = "OP_RESERVED1";
[@bs.module "bitcoin-ops"] external op_RESERVED2 : t = "OP_RESERVED2";

[@bs.module "bitcoin-ops"] external op_1ADD : t = "OP_1ADD";
[@bs.module "bitcoin-ops"] external op_1SUB : t = "OP_1SUB";
[@bs.module "bitcoin-ops"] external op_2MUL : t = "OP_2MUL";
[@bs.module "bitcoin-ops"] external op_2DIV : t = "OP_2DIV";
[@bs.module "bitcoin-ops"] external op_NEGATE : t = "OP_NEGATE";
[@bs.module "bitcoin-ops"] external op_ABS : t = "OP_ABS";
[@bs.module "bitcoin-ops"] external op_NOT : t = "OP_NOT";
[@bs.module "bitcoin-ops"] external op_0NOTEQUAL : t = "OP_0NOTEQUAL";
[@bs.module "bitcoin-ops"] external op_ADD : t = "OP_ADD";
[@bs.module "bitcoin-ops"] external op_SUB : t = "OP_SUB";
[@bs.module "bitcoin-ops"] external op_MUL : t = "OP_MUL";
[@bs.module "bitcoin-ops"] external op_DIV : t = "OP_DIV";
[@bs.module "bitcoin-ops"] external op_MOD : t = "OP_MOD";
[@bs.module "bitcoin-ops"] external op_LSHIFT : t = "OP_LSHIFT";
[@bs.module "bitcoin-ops"] external op_RSHIFT : t = "OP_RSHIFT";

[@bs.module "bitcoin-ops"] external op_BOOLAND : t = "OP_BOOLAND";
[@bs.module "bitcoin-ops"] external op_BOOLOR : t = "OP_BOOLOR";
[@bs.module "bitcoin-ops"] external op_NUMEQUAL : t = "OP_NUMEQUAL";
[@bs.module "bitcoin-ops"]
external op_NUMEQUALVERIFY : t = "OP_NUMEQUALVERIFY";
[@bs.module "bitcoin-ops"] external op_NUMNOTEQUAL : t = "OP_NUMNOTEQUAL";
[@bs.module "bitcoin-ops"] external op_LESSTHAN : t = "OP_LESSTHAN";
[@bs.module "bitcoin-ops"] external op_GREATERTHAN : t = "OP_GREATERTHAN";
[@bs.module "bitcoin-ops"]
external op_LESSTHANOREQUAL : t = "OP_LESSTHANOREQUAL";
[@bs.module "bitcoin-ops"]
external op_GREATERTHANOREQUAL : t = "OP_GREATERTHANOREQUAL";
[@bs.module "bitcoin-ops"] external op_MIN : t = "OP_MIN";
[@bs.module "bitcoin-ops"] external op_MAX : t = "OP_MAX";

[@bs.module "bitcoin-ops"] external op_WITHIN : t = "OP_WITHIN";

[@bs.module "bitcoin-ops"] external op_RIPEMD160 : t = "OP_RIPEMD160";
[@bs.module "bitcoin-ops"] external op_SHA1 : t = "OP_SHA1";
[@bs.module "bitcoin-ops"] external op_SHA256 : t = "OP_SHA256";
[@bs.module "bitcoin-ops"] external op_HASH160 : t = "OP_HASH160";
[@bs.module "bitcoin-ops"] external op_HASH256 : t = "OP_HASH256";
[@bs.module "bitcoin-ops"] external op_CODESEPARATOR : t = "OP_CODESEPARATOR";
[@bs.module "bitcoin-ops"] external op_CHECKSIG : t = "OP_CHECKSIG";
[@bs.module "bitcoin-ops"]
external op_CHECKSIGVERIFY : t = "OP_CHECKSIGVERIFY";
[@bs.module "bitcoin-ops"] external op_CHECKMULTISIG : t = "OP_CHECKMULTISIG";
[@bs.module "bitcoin-ops"]
external op_CHECKMULTISIGVERIFY : t = "OP_CHECKMULTISIGVERIFY";

[@bs.module "bitcoin-ops"] external op_NOP1 : t = "OP_NOP1";

[@bs.module "bitcoin-ops"] external op_NOP2 : t = "OP_NOP2";
[@bs.module "bitcoin-ops"]
external op_CHECKLOCKTIMEVERIFY : t = "OP_CHECKLOCKTIMEVERIFY";

[@bs.module "bitcoin-ops"] external op_NOP3 : t = "OP_NOP3";
[@bs.module "bitcoin-ops"]
external op_CHECKSEQUENCEVERIFY : t = "OP_CHECKSEQUENCEVERIFY";

[@bs.module "bitcoin-ops"] external op_NOP4 : t = "OP_NOP4";
[@bs.module "bitcoin-ops"] external op_NOP5 : t = "OP_NOP5";
[@bs.module "bitcoin-ops"] external op_NOP6 : t = "OP_NOP6";
[@bs.module "bitcoin-ops"] external op_NOP7 : t = "OP_NOP7";
[@bs.module "bitcoin-ops"] external op_NOP8 : t = "OP_NOP8";
[@bs.module "bitcoin-ops"] external op_NOP9 : t = "OP_NOP9";
[@bs.module "bitcoin-ops"] external op_NOP10 : t = "OP_NOP10";

[@bs.module "bitcoin-ops"] external op_PUBKEYHASH : t = "OP_PUBKEYHASH";
[@bs.module "bitcoin-ops"] external op_PUBKEY : t = "OP_PUBKEY";
[@bs.module "bitcoin-ops"] external op_INVALIDOPCODE : t = "OP_INVALIDOPCODE";

let numbers =
  Belt.Array.getExn([|
    op_0,
    op_1,
    op_2,
    op_3,
    op_4,
    op_5,
    op_6,
    op_7,
    op_8,
    op_9,
    op_10,
    op_11,
    op_12,
    op_13,
    op_14,
    op_15,
    op_16,
  |]);
