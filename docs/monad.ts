// Monad illustration
// https://www.youtube.com/watch?v=C2w45qRc3aU

type NumberWithLogs = {
  num: number;
  logs: Array<string>;
};

function addOne(x: NumberWithLogs): NumberWithLogs {
  const _new = {
    num: x.num + 1,
    logs: [`Added one to ${x.num} => ${x.num + 1}`],
  };

  return {
    num: _new.num,
    logs: [..._new.logs, ...x.logs],
  };
}

function square(x: NumberWithLogs): NumberWithLogs {
  const _new = {
    num: x.num * x.num,
    logs: [`Squared ${x.num} => ${x.num * x.num}`],
  };
  return {
    num: _new.num,
    logs: [..._new.logs, ...x.logs],
  };
}

function wrapWithLogs(x: number): NumberWithLogs {
  return {
    num: x,
    logs: [],
  };
}

square(addOne(wrapWithLogs(5)));

// NEWW
function square_v2(x: number): NumberWithLogs {
  return {
    num: x * x,
    logs: [`Squared ${x} => ${x * x}`],
  };
}

function addOne_ve(x: number): NumberWithLogs {
  return {
    num: x + 1,
    logs: [`Added one to ${x} => ${x + 1}`],
  };
}

function runWithLogs(
  input: NumberWithLogs,
  transformer: (_: number) => NumberWithLogs
): NumberWithLogs {
  const _new = transformer(input.num);
  return {
    num: _new.num,
    logs: [..._new.logs, ...input.logs],
  };
}

const wrappedNum = wrapWithLogs(5);
const addedOne = runWithLogs(wrappedNum, addOne_ve);
const quared = runWithLogs(addedOne, square_v2);

console.log(quared);

/**
 * Monad
 *
 * 1. Wrapper Type
 * 2. Wrap Function a.k.a. return, pure, unit
 * 3. Run Function (a.k.a. bind, flatmap, >>=)
 *      - Takes transformations on monadic values
 *      - Transformations accept unwrapped type and returns
 *        wrapper type
 *
 */

/**
 * Option type - a Monad
 *
 * 1. Wrapper Type: Option<T>
 * 2. Wrap Function: Some<T>(x: T): Option<T>
 * 3. Run Function: Run<T>(input: Option<T>, transform: (_: T) => Option<T>): Option<T>
 *  if (input == none) {
 *      return none
 *  }
 *  return transform(input.value)
 */

function getPetNickName(): string | undefined {
  const user: { name: string } | undefined = { name: "" };
  if (user == undefined) {
    return undefined;
  }

  const userPet: { name: string } | undefined = { name: "" };
  if (userPet == undefined) {
    return undefined;
  }

  const userPetNickName: string | undefined = "";
  return userPetNickName;
}

// Monadic operations can be expressed in the similar way as their non-monadic counterparts
function getPetNickName2(): Option<string> {
  const user: Option<User> = getCurrentUser();
  const userPet: Option<Pet> = run(user, getPet);
  const userPetNickname: Option<string> = run(userPet, getNickName);
  return userPetNickname;
}

function getPetNickNameNormal(): string {
  const user: User = getCurrentUser();
  const userPet: Pet = getPet(user);
  const userPetNickName: string = getNickname(userPet);
  return userPetNickName;
}

// Futures/Promises is another type of monad

const doors = ["red", "green", "blue"];

function runWithPossibilities(
  list: Array<string>,
  transform: (_: string) => Array<string>
) {
  let final = [] as Array<string>;
  list.map(transform).forEach((pairs) => {
    final = [...pairs, ...final];
  });

  return final;
}

const doorAndCoinPossibilities = runWithPossibilities(doors, (door) => [
  door + "heads",
  door + " tails",
]);

// Better way to achieve above with flatMap
const doorAndCoinPossibilities2 = doors.flatMap((door) => [
  door + " head",
  door + " tail",
]);

// Lists are monad!
