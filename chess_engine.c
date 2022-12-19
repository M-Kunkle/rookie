// Credit to maksimKorzh for the Bit Board Chess engine he made in C
// https://github.com/maksimKorzh/chess_programming/tree/master/src/bbc

#include <stdio.h>
#include <string.h>

#define U64 unsigned long long

// macros for bit manipulation
#define get_bit(bitboard, square) (bitboard & (1ULL << square))
#define set_bit(bitboard, square) (bitboard |= (1ULL << square))
#define pop_bit(bitboard, square) (get_bit(bitboard, square) ? bitboard ^= (1ULL << square) : 0)

// ------------------------------------------
// function declarations
void printboard(U64 bitboard);
U64 mask_pawn_attacks(int side, int square);
U64 mask_knight_attacks(int square);
U64 mask_king_attacks(int square);
U64 mask_bishop_attacks(int square);
U64 mask_rook_attacks(int square);
U64 bishop_attack_with_block(int square, U64 block);
U64 rook_attack_with_block(int square, U64 block);
void init_leapers_attacks(void);
int count_bits(U64 bitboard);
int get_lsb_index(U64 bitboard);
U64 set_occup(int index, int bits_in_mask, U64 attack_mask);
unsigned int random_u32(void);
U64 random_u64(void);
U64 generate_magic_num(void);
U64 find_magic_num(int square, int relevant_bits, int bishop);
void init_magic_nums(void);
void init_all(void);
void init_sliders_attacks(int bishop);
U64 get_rook_attacks(int square, U64 occupancy);
U64 get_bishop_attacks(int square, U64 occupancy);
// ------------------------------------------


// ------------------------------------------
//  quality of life variables
// ------------------------------------------
enum {
    a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1
};

const char *square_to_coord[] = {
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
    "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
    "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
};

enum { white, black };
enum { rook, bishop };

// -------------------------------------------
// random numbers
// -------------------------------------------
unsigned int state = 1804289383; // seed 

unsigned int random_u32(){
  unsigned int x = state;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  state = x;
  return x;
} 

U64 random_u64(){
  U64 n1, n2, n3, n4;

  // init random numbers slicing 16 bit from MSB side (MSB side is bottom up)
  n1 = (U64)(random_u32()) & 0xFFFF;
  n2 = (U64)(random_u32()) & 0xFFFF;
  n3 = (U64)(random_u32()) & 0xFFFF;
  n4 = (U64)(random_u32()) & 0xFFFF; 

  return n1 | (n2 << 16) | (n3 << 32) | (n4 << 48);
}

U64 generate_magic_num(){
  return random_u64() & random_u64() & random_u64();
}

// ------------------------------------------
// bit manipulation and board output
// ------------------------------------------

// count bits on the current board
int count_bits(U64 bitboard){
  int bitcount = 0;

  while(bitboard){
    bitcount++;
    bitboard &= bitboard - 1;
  }

  return bitcount;
}

// get least significant bit index
int get_lsb_index(U64 bitboard){
  int index;
  if(bitboard){
    index = count_bits((bitboard & - bitboard)-1);
    //return index;
  } else {
    return -1;
  }  
}

// print board and board number
void printboard(U64 bitboard){
  //Serial.println("\n");

  for (int rank=0; rank<8; rank++){
    for(int file=0; file<8; file++){
      int square = rank*8 + file;

      // print rank number layout
      if(!file){
        printf("  %d ", 8 - rank);
      }

      printf("  %d", get_bit(bitboard, square) ? 1 : 0);
    }
    printf("\n");
  }

  // print file layout
  printf("\n      a  b  c  d  e  f  g  h\n\n");
  printf("     Bitboard: %llud\n\n", bitboard);

}

// ----------------------------------------
// magic numbers and relevant bits
// ----------------------------------------
const int bishop_relevant_bits[64] = {
  6,  5,  5,  5,  5,  5,  5,  6,
  5,  5,  5,  5,  5,  5,  5,  5,
  5,  5,  7,  7,  7,  7,  5,  5,
  5,  5,  7,  9,  9,  7,  5,  5,
  5,  5,  7,  9,  9,  7,  5,  5,
  5,  5,  7,  7,  7,  7,  5,  5,
  5,  5,  5,  5,  5,  5,  5,  5,
  6,  5,  5,  5,  5,  5,  5,  6
};

const int rook_relevant_bits[64] = {
  12,  11,  11,  11,  11,  11,  11,  12,
  11,  10,  10,  10,  10,  10,  10,  11,
  11,  10,  10,  10,  10,  10,  10,  11,
  11,  10,  10,  10,  10,  10,  10,  11,
  11,  10,  10,  10,  10,  10,  10,  11,
  11,  10,  10,  10,  10,  10,  10,  11,
  11,  10,  10,  10,  10,  10,  10,  11,
  12,  11,  11,  11,  11,  11,  11,  12
};

const U64 rook_magic_nums[64] = {
0x80008120504000ULL,
 0xc40100040082000ULL,
 0x100102001000840ULL,
 0x1080041000080080ULL,
 0x80040080020800ULL,
 0x80020004000180ULL,
 0xa400080084300102ULL,
 0x880012041000080ULL,
 0x1200800040009020ULL,
 0x90804004601000c8ULL,
 0x1001020024100ULL,
 0x853001000892100ULL,
 0x62100120801002cULL,
 0x802000422000810ULL,
 0x8004000810520104ULL,
 0x601a000204008041ULL,
 0x4080888000400020ULL,
 0x40002000281000ULL,
 0x802820042002013ULL,
 0x1010008237000ULL,
 0x21cd10005002800ULL,
 0x401808002002400ULL,
 0x4080540001100a08ULL,
 0x4016200008100ccULL,
 0x844080208014c000ULL,
 0xc0200540005000ULL,
 0x20004100502100ULL,
 0x2000090100500020ULL,
 0x4002480280081ULL,
 0x21000300080c00ULL,
 0x800010040008ca19ULL,
 0x2401034200210284ULL,
 0x284010800480ULL,
 0x200083804004ULL,
 0x100815000802000ULL,
 0x108008882801000ULL,
 0x8000826800802400ULL,
 0x8000203008010440ULL,
 0x80010010c000802ULL,
 0x220081020001c4ULL,
 0x9182400080208004ULL,
 0x4004804102020021ULL,
 0x100802a4002000ULL,
 0x6000624112020008ULL,
 0x4814000800808004ULL,
 0x880020004008080ULL,
 0x2004021021040008ULL,
 0x824481120004ULL,
 0x8180264100800100ULL,
 0x8040011090439900ULL,
 0x520120824a00ULL,
 0x400241000090100ULL,
 0x4000802048080ULL,
 0x420090090c0200ULL,
 0x1861000200040100ULL,
 0x8000042100408200ULL,
 0x100c020120082ULL,
 0x20400024130483ULL,
 0x4060f0414d002001ULL,
 0x8021002410002821ULL,
 0x8006002068041002ULL,
 0x43b000400080211ULL,
 0x100901102080984ULL,
 0x804004821048c02ULL,
};

const U64 bishop_magic_nums[64] = {
0x40040822862081ULL,
 0x4011802028400ULL,
 0x14034401000410ULL,
 0x8204242840040ULL,
 0x488404200a000040ULL,
 0x2010420000400ULL,
 0x20150807042000a0ULL,
 0x22010108410402ULL,
 0x80908218411ULL,
 0x8228300101010200ULL,
 0x5846800810902ULL,
 0x4000820a0200000ULL,
 0x2840504254104ULL,
 0x4806091018020cULL,
 0x608508184202004ULL,
 0x21000c2098280819ULL,
 0x2020004242020200ULL,
 0x4102100490040101ULL,
 0x114012208001500ULL,
 0x108000682004460ULL,
 0x7809000490401000ULL,
 0x8c02001120900808ULL,
 0x4024016100821001ULL,
 0x41004024050420ULL,
 0x84440422002c400ULL,
 0x119111094040810ULL,
 0x4404480810048010ULL,
 0x42011000802400cULL,
 0x8001001009004000ULL,
 0x4010108001004128ULL,
 0x600202009402c204ULL,
 0x210848182021280ULL,
 0x12021000c01120ULL,
 0x1a482a00041020ULL,
 0x1002404800100930ULL,
 0x2008020420200ULL,
 0x20040c0002c102ULL,
 0x6080200804050ULL,
 0x9a82089908440401ULL,
 0x38050046102202ULL,
 0x188084884400911ULL,
 0x4008249009020ULL,
 0x1c02001048200401ULL,
 0x6002520214041a02ULL,
 0x2800401091000200ULL,
 0x44910051001200ULL,
 0x2018080860420082ULL,
 0xd003410101000a02ULL,
 0x20088a18208c0040ULL,
 0x12a010101100100ULL,
 0x4241d4100310ULL,
 0x4000008c240010ULL,
 0xd048282060410880ULL,
 0xcd82401002062100ULL,
 0x288c20806240014ULL,
 0x1820843102002050ULL,
 0x8200c844100804ULL,
 0x1109460a4102800ULL,
 0x100020004c140400ULL,
 0x42070002840404ULL,
 0x350020046404ULL,
 0x2400810850030a00ULL,
 0x1015060a1092212ULL,
 0x20202444802040ULL,
};


// ----------------------------------------
// attacks and attack table generation
// ----------------------------------------

// board constants for proper move generation
const U64 not_a_file = 18374403900871474942ULL;   
const U64 not_h_file = 9187201950435737471ULL;
const U64 not_hg_file = 4557430888798830399ULL;
const U64 not_ab_file = 18229723555195321596ULL;

// attack initialization
U64 pawn_attacks[2][64];
U64 knight_attacks[64];
U64 king_attacks[64];
U64 bishop_masks[64];
U64 rook_masks[64];

// [square][occupancies]
U64 bishop_attacks[64][512];
U64 rook_attacks[64][4096];

// pawn attack table generation
U64 mask_pawn_attacks(int side, int square){
  U64 attacks = 0ULL;
  U64 bitboard = 0ULL;

  set_bit(bitboard, square);

  if(!side){
    // white pawns
    if((bitboard >> 7) & not_a_file){
      attacks |= (bitboard >> 7);
    }
    if ((bitboard >> 9) & not_h_file){
      attacks |= (bitboard >> 9);
    }
  } else {
    // black pawns
    if((bitboard << 7) & not_h_file){
      attacks |= (bitboard << 7);
    }
    if ((bitboard << 9) & not_a_file){
      attacks |= (bitboard << 9);
    }

  }

  return attacks;
}

// knight attack table generation
U64 mask_knight_attacks(int square){
  U64 attacks = 0ULL;
  U64 bitboard = 0ULL;

  set_bit(bitboard, square);

  if((bitboard >> 17) & not_h_file){
    attacks |= (bitboard >> 17);
  }
  if((bitboard >> 15) & not_a_file){
    attacks |= (bitboard >> 15);
  }
  if((bitboard >> 10) & not_hg_file){
    attacks |= (bitboard >> 10);
  }
  if((bitboard >> 6) & not_ab_file){
    attacks |= (bitboard >> 6);
  }

  if((bitboard << 17) & not_a_file){
    attacks |= (bitboard << 17);
  }
  if((bitboard << 15) & not_h_file){
    attacks |= (bitboard << 15);
  }
  if((bitboard << 10) & not_ab_file){
    attacks |= (bitboard << 10);
  }
  if((bitboard << 6) & not_hg_file){
    attacks |= (bitboard << 6);
  }
  return attacks;
}

// king attack table generation
U64 mask_king_attacks(int square){
  U64 attacks = 0ULL;
  U64 bitboard = 0ULL;

  set_bit(bitboard, square);

  if((bitboard >> 1) & not_h_file){
    attacks |= (bitboard >> 1);
  }
  if((bitboard << 1) & not_a_file){
    attacks |= (bitboard<< 1);
  }
  if((bitboard >> 7) & not_a_file){
    attacks |= (bitboard >> 7);
  }
  if((bitboard >> 9) & not_h_file){
    attacks |= (bitboard >> 9);
  }
  if((bitboard << 7) & not_h_file){
    attacks |= (bitboard << 7);
  }
  if((bitboard << 9) & not_a_file){
    attacks |= (bitboard << 9);
  }
  attacks |= (bitboard >> 8);
  attacks |= (bitboard << 8);

  return attacks;
}

// bishop attack table generation (without edges)
U64 mask_bishop_attacks(int square){
  U64 attacks = 0ULL;
  int r, f; // rank, file
  int tr = square / 8; // target rank
  int tf = square % 8; // target file

  for (r = tr + 1, f = tf+1; r <= 6 && f <= 6; r++, f++){
    attacks |= (1ULL << (r * 8 + f));
  }
  for (r = tr - 1, f = tf+1; r >= 1 && f <= 6; r--, f++){
    attacks |= (1ULL << (r * 8 + f));
  }
  for (r = tr + 1, f = tf-1; f >= 1 && r <= 6; r++, f--){
    attacks |= (1ULL << (r * 8 + f));
  }
  for (r = tr - 1, f = tf-1; f >= 1 && r >= 1; r--, f--){
    attacks |= (1ULL << (r * 8 + f));
  }

  return attacks;
}

// rook attack table generation (without edges)
U64 mask_rook_attacks(int square){
  U64 attacks = 0ULL;
  int r, f; // rank, file
  int tr = square / 8; // target rank
  int tf = square % 8; // target file

  for(r = tr + 1; r <= 6; r++){
    attacks |= (1ULL << (r * 8 + tf));
  }
  for(r = tr - 1; r >= 1; r--){
    attacks |= (1ULL << (r * 8 + tf));
  }
  for(f = tf + 1; f <= 6; f++){
    attacks |= (1ULL << (tr * 8 + f));
  }
  for(f = tf - 1; f >= 1; f--){
    attacks |= (1ULL << (tr * 8 + f));
  }
  
  return attacks;
}

// bishop attack table with blocked paths accounted for
U64 bishop_attack_with_block(int square, U64 block){
  U64 attacks = 0ULL;
  int r, f; // rank, file
  int tr = square / 8; // target rank
  int tf = square % 8; // target file

  for (r = tr + 1, f = tf+1; r <= 7 && f <= 7; r++, f++){
    attacks |= (1ULL << (r * 8 + f));
    if((1ULL << (r * 8 + f) & block)){
      break;
    }
  }
  for (r = tr - 1, f = tf+1; r >= 0 && f <= 7; r--, f++){
    attacks |= (1ULL << (r * 8 + f));
    if((1ULL << (r * 8 + f) & block)){
      break;
    }
  }
  for (r = tr + 1, f = tf-1; f >= 0 && r <= 7; r++, f--){
    attacks |= (1ULL << (r * 8 + f));
    if((1ULL << (r * 8 + f) & block)){
      break;
    }
  }
  for (r = tr - 1, f = tf-1; f >= 0 && r >= 0; r--, f--){
    attacks |= (1ULL << (r * 8 + f));
    if((1ULL << (r * 8 + f) & block)){
      break;
    }
  }

  return attacks;
}

// rook attack table with blocked paths accounted for
U64 rook_attack_with_block(int square, U64 block){
  U64 attacks = 0ULL;
  int r, f; // rank, file
  int tr = square / 8; // target rank
  int tf = square % 8; // target file

  for(r = tr + 1; r <= 7; r++){
    attacks |= (1ULL << (r * 8 + tf));
    if((1ULL << (r * 8 + tf) & block)){
      break;
    }
  }
  for(r = tr - 1; r >= 0; r--){
    attacks |= (1ULL << (r * 8 + tf));
    if((1ULL << (r * 8 + tf) & block)){
      break;
    }
  }
  for(f = tf + 1; f <= 7; f++){
    attacks |= (1ULL << (tr * 8 + f));
    if((1ULL << (tr * 8 + f) & block)){
      break;
    }
  }
  for(f = tf - 1; f >= 0; f--){
    attacks |= (1ULL << (tr * 8 + f));
    if((1ULL << (tr * 8 + f) & block)){
      break;
    }
  }
  
  return attacks;
}

U64 get_bishop_attacks(int square, U64 occupancy){
  occupancy &= bishop_masks[square];
  occupancy *= bishop_magic_nums[square];
  occupancy >>= 64 - bishop_relevant_bits[square];

  return bishop_attacks[square][occupancy];
}

U64 get_rook_attacks(int square, U64 occupancy){
  occupancy &= rook_masks[square];
  occupancy *= rook_magic_nums[square];
  occupancy >>= 64 - rook_relevant_bits[square];

  return rook_attacks[square][occupancy];
}


// set occupied squares
U64 set_occup(int index, int bits_in_mask, U64 attack_mask){
  U64 map = 0ULL;

  for(int count = 0; count < bits_in_mask; count++){
    int square = get_lsb_index(attack_mask);
    pop_bit(attack_mask, square);

    if(index & (1 << count)){
      map |= (1ULL << square);
    }
  }

  return map;
}

// ----------------------------------------
// magic number generation
// ----------------------------------------

U64 find_magic_num(int square, int relevant_bits, int bishop){
  U64 occupancies[4096];
  U64 attacks[4096];
  U64 used_attacks[4096];
  U64 attack_mask = bishop ? mask_bishop_attacks(square) : mask_rook_attacks(square);

  int occupancy_indices = 1 << relevant_bits;
  for(int idx = 0; idx < occupancy_indices; idx++){
    occupancies[idx] = set_occup(idx, relevant_bits, attack_mask);
    attacks[idx] = bishop ? bishop_attack_with_block(square, occupancies[idx]) : rook_attack_with_block(square, occupancies[idx]); 
  }

  // test magic nums
  for(int i = 0; i < 100000000; i++){
    U64 magic_num = generate_magic_num();
    if(count_bits((attack_mask * magic_num) & 0xFF00000000000000 < 6)){
      continue;
    }

    memset(used_attacks, 0ULL, sizeof(used_attacks));

    int index, fail;
    for(index = 0, fail = 0; !fail && index < occupancy_indices; index++){
      int magic_idx = (int)((occupancies[index] * magic_num) >> (64 - relevant_bits));
      if(used_attacks[magic_idx] == 0ULL){
        used_attacks[magic_idx] = attacks[index];
      }
      else if(used_attacks[magic_idx] != attacks[index]){
        fail = 1;
      }
    }

    if(!fail){
      return magic_num;
    }
  }
  printf("  Magic # fails!");
    return 0ULL;
}

void init_magic_nums(){
  for(int square = 0; square < 64; square++){
    printf(" 0x%llxULL,\n", find_magic_num(square, rook_relevant_bits[square], rook));
    printf(" 0x%llxULL,\n", find_magic_num(square, bishop_relevant_bits[square], bishop));
  }
}

// ----------------------------------------
// intializations
// ----------------------------------------


void init_leapers_attacks(){
  for (int square=0; square<64; square++){
    pawn_attacks[white][square] = mask_pawn_attacks(white, square);
    pawn_attacks[black][square] = mask_pawn_attacks(black, square);
    knight_attacks[square] = mask_knight_attacks(square);
    king_attacks[square] = mask_king_attacks(square);
  }
}

void init_sliders_attacks(int bishop){
  for(int square = 0; square < 64; square++){
    bishop_masks[square] = mask_bishop_attacks(square);
    rook_masks[square] = mask_rook_attacks(square);

    U64 attack_mask = bishop ? bishop_masks[square] : rook_masks[square];
    int relevant_bits = count_bits(attack_mask);
    int occupancy_indices = (1 << relevant_bits);

    for(int idx = 0; idx < occupancy_indices; idx++){
      // BISHOP
      if(bishop){
        U64 occupancy = set_occup(idx, relevant_bits, attack_mask);
        int magic_idx = (occupancy * bishop_magic_nums[square]) >> (64 - bishop_relevant_bits[square]);

        bishop_attacks[square][magic_idx] = bishop_attack_with_block(square, occupancy);
      } else {
        // ROOK
        U64 occupancy = set_occup(idx, relevant_bits, attack_mask);
        int magic_idx = (occupancy * rook_magic_nums[square]) >> (64 - rook_relevant_bits[square]);

        rook_attacks[square][magic_idx] = rook_attack_with_block(square, occupancy);
      }
    }
  }
}

void init_all(){
  init_leapers_attacks();
  init_sliders_attacks(bishop);
  init_sliders_attacks(rook);
}

// MAIN ----------------------------------------------------

int main(){
  init_all();

  U64 testboard = 14073749976403968ULL;

  printboard(testboard);
  printboard(get_rook_attacks(f7, testboard));
  printboard(get_bishop_attacks(f7, testboard));

  return 0;
}