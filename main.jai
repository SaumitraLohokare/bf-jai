#import "Basic";
#import "File";
#import "Posix";

usage :: (program: string) {
	print("\nUSAGE: % <file> [<file> ...]\n", program);
}

main :: () {
	args := get_command_line_arguments();

	if args.count < 2 {
		print("ERROR: Please provide a brainf*ck file.\n");
		usage(args[0]);
		exit(1);
	}
	
	for i: 1..args.count-1 {
		arg := args[i];

		content, ok := read_entire_file(arg);
		if !ok {
			print("ERROR: Could not find '%'\n", arg);
			exit(1);
		}

		print("Simulating %...\n", arg);
		program := parse_src(content);
		simulate_program(program);
		putchar(#char "\n"); // Flush STDOUT
		
		free(program.data);
		free(content);
	}
}

Instruction :: enum u8 {
	Left;
	Right;
	Dec;
	Inc;
	Get;
	Put;
	Loop;
	End;
}

parse_src :: (src: string) -> []Instruction {
	instructions: [..]Instruction;

	for src {
		if it == {
			case #char "<"; array_add(*instructions, .Left);
			case #char ">"; array_add(*instructions, .Right);
			case #char "-"; array_add(*instructions, .Dec);
			case #char "+"; array_add(*instructions, .Inc);
			case #char ","; array_add(*instructions, .Get);
			case #char "."; array_add(*instructions, .Put);
			case #char "["; array_add(*instructions, .Loop);
			case #char "]"; array_add(*instructions, .End);
		}
	}

	return instructions;
}

simulate_program :: (program: []Instruction) {
	assert(program.count > 0);
	index := 0;
	tape: [1000]s32;
	pc := 0;

	while pc < program.count {
		inst := program[pc];
		pc += 1;
		
		if inst == {
			case .Left; {
				if index == 0 {
					print("ERROR: Moved left past 0.\n");
					exit(1);
				}
				index -= 1;
			}
			case .Right; {
				index += 1;
				if index >= 1000 {
					print("ERROR: Tape out of bounds.\n");
					exit(1);
				}
			}
			case .Inc;{
				tape[index] += 1;
			}
			case .Dec; {
				tape[index] -= 1;
			}
			case .Get; {
				print("Enter Char: ");
				tape[index] = getchar();
			}
			case .Put; {
				putchar(tape[index]);
			}
			case .Loop; {
				if tape[index] == 0 {
					pc = goto_end(program, pc-1);
					assert(index != -1);
				}
			}
			case .End; {
				if tape[index] != 0 {
					pc = goto_loop(program, pc-1);
					assert(index != -1);
				}
			}
		}
	}
}

goto_end :: (program: []Instruction, index: s64) -> s64 {
	assert(program[index] == .Loop);

	stack := 0;
	for index..program.count-1 {
		inst := program[it];
		if inst == .Loop {
			stack += 1;
		} else if inst == .End {
			stack -= 1;
		}

		if stack == 0 {
			return index + 1; // Instruction after `]`
		}
	}

	return -1;
}

goto_loop :: (program: []Instruction, index: s64) -> s64 {
	assert(program[index] == .End);

	stack := 0;
	while index > 0 {
		inst := program[index];

		if inst == .End {
			stack += 1;
		} else if inst == .Loop {
			stack -= 1;
		}

		if stack == 0 {
			return index + 1; // Instruction after `[`
		}

		index -= 1;
	}

	return -1;
}
