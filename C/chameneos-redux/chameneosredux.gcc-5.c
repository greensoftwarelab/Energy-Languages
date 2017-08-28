/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/

Contributed by Dmitry Vyukov

Kernel thread is created for each chameneous.
Atomic compare-and-swap primitive is used 
for meeting place state manipulation.
*/

#define _GNU_SOURCE
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>
#include <pthread.h>
#include <sched.h>

#define CPUINFO_FILENAME "/proc/cpuinfo"

#define CL_SIZE 64

void* cache_aligned_malloc(size_t sz)
{
    char*                       mem;
    char*                       res;
    void**                      pos;

    mem = (char*)malloc(sz + 2 * CL_SIZE);
    if (mem == 0)
        exit(1);
    res = (char*)((uintptr_t)(mem + CL_SIZE) & ~(CL_SIZE - 1));
    pos = (void**)(res - sizeof(void*));
    pos[0] = mem;
    return res;
}

void cache_aligned_free(void* res)
{
    void*                       mem;
    void**                      pos;

    assert(((uintptr_t)res & (CL_SIZE - 1)) == 0);
    pos = (void**)((char*)res - sizeof(void*));
    mem = pos[0];
    free(mem);
}

enum color_t
{
    color_blue,
    color_red,
    color_yellow,
};

char const* color_names[] = {"blue", "red", "yellow"};

enum color_t color_complement(enum color_t c1, enum color_t c2)
{
   switch (c1)
   {
   case color_blue:
      switch (c2)
      {
      case color_blue:      return color_blue;
      case color_red:       return color_yellow;
      case color_yellow:    return color_red;
      }
   case color_red:
      switch (c2)
      {
      case color_blue:      return color_yellow;
      case color_red:       return color_red;
      case color_yellow:    return color_blue;
      }
   case color_yellow:
      switch (c2)
      {
      case color_blue:      return color_red;
      case color_red:       return color_blue;
      case color_yellow:    return color_yellow;
      }
   }
   assert(0);
   return 0;
}

void print_colors()
{
    enum color_t                c1;
    enum color_t                c2;
    enum color_t                c3;

    for (c1 = color_blue; c1 <= color_yellow; c1 += 1)
    {
        for (c2 = color_blue; c2 <= color_yellow; c2 += 1)
        {
            c3 = color_complement(c1, c2);
            printf("%s + %s -> %s\n",
                color_names[c1], color_names[c2], color_names[c3]);
        }
    }
    printf("\n");
}

char const* spell_number(size_t n)
{
    static char                 buf [128];
    static char const*          numbers [] = {
        " zero", " one", " two",   " three", " four",
        " five", " six", " seven", " eight", " nine"};

    size_t                      tokens [32];
    size_t                      token_count;
    char const*                 tok;
    char*                       pos;

    token_count = 0;
    do
    {
        tokens[token_count] = n % 10;
        token_count += 1;
        n /= 10;
    }
    while (n);

    pos = buf;
    while (token_count)
    {
        token_count -= 1;
        tok = numbers[tokens[token_count]];
        while (tok[0])
            pos++[0] = tok++[0];
    }
    pos[0] = 0;
    return buf;
}

struct meeting_place_t
{
    uintptr_t volatile          state;
};

#define CHAMENEOS_IDX_MASK      0xFF
#define MEET_COUNT_SHIFT        8

struct chameneos_t
{
    enum color_t                color;
    size_t                      meet_count;
    size_t                      meet_same_count;
    int volatile                meeting_completed;
    struct meeting_place_t*     place;
    struct chameneos_t**        chameneos;
    size_t                      id;
    int                         is_smp;
    pthread_t                   thread;
    pthread_attr_t              thread_attr;
};

void* chameneos_func(void* ctx)
{
    struct chameneos_t*         chameneos;
    struct chameneos_t**        chameneoses;
    struct chameneos_t*         peer;
    size_t                      my_id;
    size_t                      is_same;
    size_t                      spin_count;
    uintptr_t volatile*         state_p;
    uintptr_t                   state;
    uintptr_t                   peer_idx;
    uintptr_t                   xchg;
    uintptr_t                   prev;
    enum color_t                new_color;
    int                         is_smp;

    chameneos = (struct chameneos_t*)ctx;
    chameneoses = chameneos->chameneos;
    state_p = &chameneos->place->state;
    my_id = chameneos->id;
    is_smp = chameneos->is_smp;

    state = state_p[0];
    for (;;)
    {
        peer_idx = state & CHAMENEOS_IDX_MASK;
        if (peer_idx)
            xchg = state - peer_idx - (1 << MEET_COUNT_SHIFT);
        else if (state)
            xchg = state | my_id;
        else
            break;
        prev = __sync_val_compare_and_swap(state_p, state, xchg);
        if (prev == state)
        {
            if (peer_idx)
            {
                is_same = (peer_idx == my_id);
                peer = chameneoses[peer_idx - 1];
                new_color = color_complement(chameneos->color, peer->color);
                peer->color = new_color;
                peer->meet_count += 1;
                peer->meet_same_count += is_same;
                peer->meeting_completed = 1;
                chameneos->color = new_color;
                chameneos->meet_count += 1;
                chameneos->meet_same_count += is_same;
            }
            else
            {
                if (is_smp)
                {
                    spin_count = 20000;
                    while (chameneos->meeting_completed == 0)
                    {
                        if (spin_count)
                            spin_count -= 1;
                        else
                            sched_yield();
                    }
                }
                else
                {
                    while (chameneos->meeting_completed == 0)
                    {
                        sched_yield();
                    }
                }
                chameneos->meeting_completed = 0;
                state = state_p[0];
            }
        }
        else
        {
            state = prev;

        }
    }
    return 0;
}

void get_affinity(int* is_smp, cpu_set_t* affinity1, cpu_set_t* affinity2)
{
    cpu_set_t                   active_cpus;
    FILE*                       f;
    char                        buf [2048];
    char const*                 pos;
    int                         cpu_idx;
    int                         physical_id;
    int                         core_id;
    int                         cpu_cores;
    int                         apic_id;
    size_t                      cpu_count;
    size_t                      i;

    char const*                 processor_str       = "processor";
    size_t                      processor_str_len   = strlen(processor_str);
    char const*                 physical_id_str     = "physical id";
    size_t                      physical_id_str_len = strlen(physical_id_str);
    char const*                 core_id_str         = "core id";
    size_t                      core_id_str_len     = strlen(core_id_str);
    char const*                 cpu_cores_str       = "cpu cores";
    size_t                      cpu_cores_str_len   = strlen(cpu_cores_str);
    
    CPU_ZERO(&active_cpus);
    sched_getaffinity(0, sizeof(active_cpus), &active_cpus);
    cpu_count = 0;
    for (i = 0; i != CPU_SETSIZE; i += 1)
    {
        if (CPU_ISSET(i, &active_cpus))
        {
            cpu_count += 1;
        }
    }

    if (cpu_count == 1)
    {
        is_smp[0] = 0;
        return;
    }

    is_smp[0] = 1;
    CPU_ZERO(affinity1);
    CPU_ZERO(affinity2);

    f = fopen(CPUINFO_FILENAME, "r");

    if (cpu_count < 4 || f == 0)
    {
        for (i = 0; i != CPU_SETSIZE; i += 1)
        {
            if (CPU_ISSET(i, &active_cpus))
            {
                CPU_SET(i, affinity1);
                CPU_SET(i, affinity2);
            }
        }
        return;
    }

    cpu_idx = physical_id = core_id = cpu_cores = -1;
    while (fgets(buf, 2048, f))
    {
        if (0 == strncmp(buf, processor_str, processor_str_len))
        {
            pos = strchr(buf + processor_str_len, ':');
            if (pos)
                cpu_idx = atoi(pos + 1);
        }
        else if (0 == strncmp(buf, physical_id_str, physical_id_str_len))
        {
            pos = strchr(buf + physical_id_str_len, ':');
            if (pos)
                physical_id = atoi(pos + 1);
        }
        else if (0 == strncmp(buf, core_id_str, core_id_str_len))
        {
            pos = strchr(buf + core_id_str_len, ':');
            if (pos)
                core_id = atoi(pos + 1);
        }
        else if (0 == strncmp(buf, cpu_cores_str, cpu_cores_str_len))
        {
            pos = strchr(buf + cpu_cores_str_len, ':');
            if (pos)
                cpu_cores = atoi(pos + 1);
        }
        if (cpu_idx >= 0 && physical_id >= 0 && core_id >= 0 && cpu_cores >= 0)
        {
            apic_id = physical_id * cpu_cores + core_id;
            if (apic_id == 0 || apic_id == 1)
                CPU_SET(cpu_idx, affinity1);
            else if (apic_id == 2 || apic_id == 3)
                CPU_SET(cpu_idx, affinity2);
            cpu_idx = physical_id = core_id = cpu_cores = -1;
        }
    }

    fclose(f);
}

void init_and_start(enum color_t* initial_colors, size_t chameneos_count,
    struct meeting_place_t** place, struct chameneos_t*** chameneos,
    size_t meet_count, int is_smp, cpu_set_t* affinity)
{
    size_t                      i;

    place[0] = (struct meeting_place_t*)
        cache_aligned_malloc(sizeof(struct meeting_place_t));
    place[0]->state = meet_count << MEET_COUNT_SHIFT;
    chameneos[0] = (struct chameneos_t**)
        cache_aligned_malloc(chameneos_count * sizeof(struct chameneos_t*));
    for (i = 0; i != chameneos_count; i += 1)
    {
        chameneos[0][i] = (struct chameneos_t*)
            cache_aligned_malloc(sizeof(struct chameneos_t));
        chameneos[0][i]->place = place[0];
        chameneos[0][i]->chameneos = chameneos[0];
        chameneos[0][i]->id = i + 1;
        chameneos[0][i]->is_smp = is_smp;
        chameneos[0][i]->meet_count = 0;
        chameneos[0][i]->meet_same_count = 0;
        chameneos[0][i]->color = initial_colors[i];
        chameneos[0][i]->meeting_completed = 0;
        if (pthread_attr_init(&chameneos[0][i]->thread_attr))
            exit(1);
        if (is_smp)
            pthread_attr_setaffinity_np(&chameneos[0][i]->thread_attr,
                sizeof(cpu_set_t), affinity);
        if (pthread_create(&chameneos[0][i]->thread,
            &chameneos[0][i]->thread_attr, chameneos_func, chameneos[0][i]))
            exit(1);
    }
}

void join_and_output(enum color_t* initial_colors, size_t chameneos_count,
    struct meeting_place_t* place, struct chameneos_t** chameneos)
{
    size_t                      total_meet_count;
    size_t                      i;

    for (i = 0; i != chameneos_count; i += 1)
        printf(" %s", color_names[initial_colors[i]]);
    printf("\n");

    for (i = 0; i != chameneos_count; i += 1)
    {
        pthread_join(chameneos[i]->thread, 0);
        pthread_attr_destroy(&chameneos[i]->thread_attr);
    }

    total_meet_count = 0;
    for (i = 0; i != chameneos_count; i += 1)
    {
        total_meet_count += chameneos[i]->meet_count;
        printf("%u%s\n", chameneos[i]->meet_count,
            spell_number(chameneos[i]->meet_same_count));
        cache_aligned_free(chameneos[i]);
    }
    printf("%s\n\n", spell_number(total_meet_count));

    cache_aligned_free(chameneos);
    cache_aligned_free(place);
}

int main(int argc, char** argv)
{
    enum color_t                initial_colors1 [] = 
        {color_blue, color_red, color_yellow};

    enum color_t                initial_colors2 [] = 
        {color_blue, color_red, color_yellow, color_red, color_yellow,
        color_blue, color_red, color_yellow, color_red, color_blue};

    struct meeting_place_t*     place1;
    struct chameneos_t**        chameneos1;
    size_t                      chameneos_count1;

    struct meeting_place_t*     place2;
    struct chameneos_t**        chameneos2;
    size_t                      chameneos_count2;

    int                         is_smp;
    cpu_set_t                   affinity1;
    cpu_set_t                   affinity2;
    size_t                      meet_count;

    meet_count = 6000000;
    if (argc > 1 && atoi(argv[1]) > 0)
        meet_count = atoi(argv[1]);

    print_colors();

    get_affinity(&is_smp, &affinity1, &affinity2);

    chameneos_count1 = sizeof(initial_colors1)/sizeof(initial_colors1[0]);
    chameneos_count2 = sizeof(initial_colors2)/sizeof(initial_colors2[0]);

    if (is_smp)
    {
        init_and_start(initial_colors1, chameneos_count1, &place1, &chameneos1, meet_count, is_smp, &affinity1);
        init_and_start(initial_colors2, chameneos_count2, &place2, &chameneos2, meet_count, is_smp, &affinity2);
        join_and_output(initial_colors1, chameneos_count1, place1, chameneos1);
        join_and_output(initial_colors2, chameneos_count2, place2, chameneos2);
    }
    else
    {
        init_and_start(initial_colors1, chameneos_count1, &place1, &chameneos1, meet_count, is_smp, &affinity1);
        join_and_output(initial_colors1, chameneos_count1, place1, chameneos1);
        init_and_start(initial_colors2, chameneos_count2, &place2, &chameneos2, meet_count, is_smp, &affinity2);
        join_and_output(initial_colors2, chameneos_count2, place2, chameneos2);
    }

    return 0;
}