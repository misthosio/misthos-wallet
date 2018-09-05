let payout = {|POLICY: This Payout requires unanimous endorsement. When all
              Partners have endorsed it, the Payout will proceed.|};

let partnerAddition = (~userId) => {j|POLICY: This proposal requires unanimous endorsement. When all Partners have endorsed this proposal, $userId will become a Partner of this Venture.|j};

let partnerRemoval = (~userId) => {j|POLICY: This proposal requires endorsement from N-1 Partners for it to become accepted. When this condition is met, $userId will no longer be a Partner of this Venture.|j};
