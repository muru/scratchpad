#! /usr/bin/awk -f

function build (block, source1, source2, sources, mixtures)
{		
	if (! source1)
	{
		for (char in sources)
		{
			if (source2 != char char)
			{
				source1 = char char
				delete sources[char]
				break
			}
		}
	}
	if (! source2)
	{	
		for (char in sources)
		{
			if (source1 != char char)
			{
				source2 = char char
				delete sources[char]
				break
			}
		}
	}
	printf "%s %s %s\n", block, "source1", source1
	printf "%s %s %s\n", block, "source2", source2
	for (m in mixtures)
	{
		for (i = 0; i < mixtures[m]; i++)
		{
			printf "%s %s %s\n", block, "mixture", m
		}
	}

}

{
	if (prev != $1)
	{
		if (prev in data)
		{
			build(prev, source1, source2, sources, mixtures)
		}

		prev = $1
		source1 = ""
		source2 = ""
		delete sources
		delete mixtures
	}

	data[$1]++
	if ($2 == "source1") {source1 = $3; next}
	if ($2 == "source2") {source2 = $3; next}
	if ($2 == "mixture")
	{
		mixtures[$3]++ 
		split ($3, chars, "")
		for (i=1; i <= length($3); i++)
		{
			sources[chars[i]]++
		}
	}
}

END { build(prev, source1, source2, sources, mixtures) }
