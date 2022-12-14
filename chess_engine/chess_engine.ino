// Credit to maksimKorzh for the Bit Board Chess engine he made in C
// I followed his guide and videos and made the necessary Arduino adjustments
// https://github.com/maksimKorzh/chess_programming/tree/master/src/bbc

#define U64 unsigned long long

// Macros for bit manipulation
#define get_bit(bitboard, square) (bitboard & (1ULL << square))
#define set_bit(bitboard, square) (bitboard |= (1ULL << square))
#define pop_bit(bitboard, square) (get_bit(bitboard, square) ? bitboard ^= (1ULL << square) : 0)


// Attack Tables
U64 pawn_attacks[2][64];
U64 knight_attacks[64];
U64 king_attacks[64];


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

enum {white, black};

void setup() {
  Serial.begin(9600);

}

void loop() {

  init_leapers_attacks();
  
  // for(int i=0; i<64;i++){
  //   printboard(mask_rook_attacks(i));
  // }
  U64 block = 0ULL;
  set_bit(block, b4);
  set_bit(block, d2);
  set_bit(block, g4);
  set_bit(block, d5);

  printboard(block);
  printboard(rook_attack_with_block(d4, block));

  for(;;);
}




// print board and board number
void printboard(U64 bitboard){
  Serial.println("\n");

  for (int rank=0; rank<8; rank++){
    for(int file=0; file<8; file++){
      int square = rank*8 + file;

      // print rank number layout
      if(!file){
        Serial.print(" ");
        Serial.print(8-rank);
        Serial.print("   ");
      }

      Serial.print(" ");
      Serial.print(get_bit(bitboard, square) ? 1 : 0);
      Serial.print(" ");
    }
    Serial.println();
  }

  // print file layout
  Serial.print("\n      a  b  c  d  e  f  g  h");
  Serial.print("\n\n BB#: ");


  // print board number
  // Serial.print does not accept ULL so used workaround
  char buf[50];
  if(bitboard > 0xFFFFFFFFLL) {
    sprintf(buf, "%lX%08lX", (unsigned long)(bitboard>>32), (unsigned long)(bitboard&0xFFFFFFFFULL));
  } else {
    sprintf(buf, "%lX", (unsigned long)bitboard);
  }
  Serial.print( buf );
  Serial.print("H\n");
}

// board constants for proper move generation
const U64 not_a_file = 18374403900871474942ULL;
const U64 not_h_file = 9187201950435737471ULL;
const U64 not_hg_file = 4557430888798830399ULL;
const U64 not_ab_file = 18229723555195321596ULL;

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

// actually generate the tables
void init_leapers_attacks(){
  for (int square=0; square<64; square++){
    pawn_attacks[white][square] = mask_pawn_attacks(white, square);
    pawn_attacks[black][square] = mask_pawn_attacks(black, square);
    knight_attacks[square] = mask_knight_attacks(square);
    king_attacks[square] = mask_king_attacks(square);
  }
}
