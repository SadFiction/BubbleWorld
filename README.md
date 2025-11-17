# BubbleWorld

University of Edinburgh FP Programming Competition.

## Bubbles

Introducing Bubble World. Using a cellular automaton and a custom ruleset on a 3 by 3 Moore neighbourhood, I tried to imitate how bubbles would flow.

On my way to watching a movie with my friends, I dropped my Marinda drink (made of glass; surprised it didn't break yikes). It fizzled up and it bubbled; that's where I got my inspiration from!

Looking at some pictures, I decided on a start ( inspiration.jpg). By blurring it up a little, I decided I could reasonably mimic the look of it!

## Ruleset

When figuring out the basic rules, I ended up on the following:

- When bubbles touch, they merge.

- Bubbles expand and grow.

- Bubbles experience some sort of pressure.

- Bubbles fade and "pop" eventually.

After some trial and error, I ended up on some specific constraints (found in Bubbles.hs). This gave me a balance between the bubbles popping too quickly and filling the whole screen (it happened way too often, very sensitive).

Finally after getting the basic ruleset down, I could modify the merge weights such that it merged towards a specific direction. Giving the impression that the bubbles flow upwards. Further on, by tying these weights to a sin function and interpolating between stages with this sin function I could make the bubbles sway.

## How to run

`cargo run` or
`cargo build` and run the exe.

Note, you need freeglut.dll installed and for it to be defined in the Path env variable.

## Issues

The biggest one; performance. For a 200 by 200 matrix, you would have to do 40000 calls of our function lambda per frame. For a 300 by 300, we have to do 90000 per frame. The issue is that since it is a matrix and we have to go through all the cells individually, the worst case scenario is our best case scenario O(n ^2). My first thought was to compute these in parallel and get their results. This yielded a major performance boost and seemed to work without issue.

Another issue was with the rendering. Initially, I wanted to use circles to render the dots so I could give a dot matrix look. However, since the circles had to be rendered using polygons, you could imagine the performance overheads. Eventually, I decided that rendering very small squares would be indistinguishable from a circle, especially at the sizes we are working on. Further on, this gave a major performance boost as well by 2-3x more.

Some other notable issues off the top of my head were:

- Calculating the sway in each lambda call was too taxing, so I moved it in the update function so it would only have to be called once per frame rather than n^2 times.

- Blissfully ignorant, I rendered all the cells with no values. Which caused cells to be rendered with invisible polygons.

- Even worse, right at the beginning, I rendered all the cells on top of the old ones. Causing a bit of an issue...

## What I would work on next

I believe that this would definitely greatly benefit from GPU processing rather than CPU processing. There's so many parallel calculations to be done. It's like rendering game frames with a CPU.

And if I had more time, I would have loved to see how it would have looked being rendered in a bottle to pay tribute to the inspiration. You could have turned the bottle around and swayed it, maybe it bubbled up more if you shook it.

## NOTES

Unfortunately, the higher the resolution, the worse the performance is ( as you can see in the videos), which can make it seem a bit like a noise machine. There's places where I could have improved it more. I even resorted to some weird inline method where the function code is ran directly rather than using a function stack. Apparently it has a bit of performance improvement which is like running one big chunk of code.

Also, looking at some submissions from past years. There was NO inspiration from them with the CA!


Thank you for reading through this,
Carlo Opincaru
