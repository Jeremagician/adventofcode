/* Could not find an elegant solution using haskell
 * this one uses C */
#include <stdio.h>
#include <stdlib.h>

static char lights[1000][1000];

struct point
{
	int x;
	int y;
};

struct rectangle
{
	struct point lower;
	struct point upper;
};


/* Conditional compilation to solve either part 1 or 2 */
#if 0
char ON (char c) { return 1; }
char OFF (char c) { return 0; }
char TOGGLE (char c) { return !c; }
#else
char ON (char c) { return c+1; }
char OFF (char c) { return c <= 0 ? 0 : c - 1; }
char TOGGLE (char c) { return c + 2; }
#endif

struct action
{
	char (*fn)(char c);
	int type;
	struct rectangle rectangle;
};

void usage(char *progname)
{
	fprintf(stderr, "%s input_file\n", progname);
}

void apply_action(struct action *action, char map[1000][1000])
{
	int i, j;
	for (i = action->rectangle.lower.x;
		 i <= action->rectangle.upper.x; i++)
	{
		for (j = action->rectangle.lower.y;
			 j <= action->rectangle.upper.y; j++)
		{
			map[i][j] = action->fn(map[i][j]);
		}
	}
}

int count_lights(char map[1000][1000])
{
	int i, j, sum = 0;

	for (i = 0; i < 1000; i++)
		for (j = 0; j < 1000; j++)
			sum += map[i][j];

	return sum;
}

void parse_action(char *line, struct action *action)
{
	if (sscanf(line, "turn off %i,%i through %i,%i",
			   &action->rectangle.lower.x,
			   &action->rectangle.lower.y,
			   &action->rectangle.upper.x,
			   &action->rectangle.upper.y) > 0)
	{
		action->fn = OFF;
		return;
	}

	if (sscanf(line, "turn on %i,%i through %i,%i",
			   &action->rectangle.lower.x,
			   &action->rectangle.lower.y,
			   &action->rectangle.upper.x,
			   &action->rectangle.upper.y) > 0)
	{
		action->fn = ON;
		return;
	}

	if (sscanf(line, "toggle %i,%i through %i,%i",
			   &action->rectangle.lower.x,
			   &action->rectangle.lower.y,
			   &action->rectangle.upper.x,
			   &action->rectangle.upper.y) > 0)
	{
		action->fn = TOGGLE;
		return;
	}
}

int main(int argc, char *argv[])
{
	FILE* input = NULL;
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	struct action action;

	if (argc != 2)
	{
		usage(argv[0]);
		return EXIT_FAILURE;
	}

	input = fopen(argv[1], "r");
	if (input == NULL)
	{
		perror(argv[1]);
		return EXIT_FAILURE;
	}

	while ((linelen = getline(&line, &linecap, input)) > 0)
	{
		parse_action(line, &action);
		apply_action(&action, lights);
	}

	printf("result: %i\n", count_lights(lights));

	free(line);

	return EXIT_SUCCESS;
}
