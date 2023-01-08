// Credit to maksimKorzh for the Bit Board Chess engine he made in C
// https://github.com/maksimKorzh/chess_programming/tree/master/src/bbc

#include <stdio.h>
#include <string.h>
#include <windows.h>

#define U64 unsigned long long

// ------------------------------------------
// macros for bit manipulation
// ------------------------------------------
#define get_bit(bitboard, square) ((bitboard) & (1ULL << (square)))
#define set_bit(bitboard, square) ((bitboard) |= (1ULL << (square)))
#define pop_bit(bitboard, square) ((bitboard) &= ~(1ULL << (square)))

// ------------------------------------------
// FEN debug strings
// ------------------------------------------
#define empty_board "8/8/8/8/8/8/8/8 w - - "
#define start_position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 "
#define tricky_position "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 "
#define killer_position "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
#define cmk_position "r2q1rk1/ppp2ppp/2n1bn2/2b1p3/3pP3/3P1NPP/PPP1NPB1/R1BQ1RK1 b - - 0 9 "

// ------------------------------------------
// move encoding macros
// ------------------------------------------

/*
          binary move bits                               hexidecimal constants
    
    0000 0000 0000 0000 0011 1111    source square       0x00003F
    0000 0000 0000 1111 1100 0000    target square       0x000FC0
    0000 0000 1111 0000 0000 0000    piece               0x00F000
    0000 1111 0000 0000 0000 0000    promoted piece      0x0F0000
    0001 0000 0000 0000 0000 0000    capture flag        0x100000
    0010 0000 0000 0000 0000 0000    double push flag    0x200000
    0100 0000 0000 0000 0000 0000    enpassant flag      0x400000
    1000 0000 0000 0000 0000 0000    castling flag       0x800000
*/

#define encode_move(source, target, piece, promoted, capture, double_move, enpass, castling) \
((source) | (target << 6) | (piece << 12) | (promoted << 16) | (capture << 20) | (double_move << 21) | \
(enpass << 22) | (castling << 23))

#define get_move_source(move) (move & 0x3F)
#define get_move_target(move) ((move & 0xFC0) >> 6)
#define get_move_piece(move) ((move & 0xF000) >> 12)
#define get_move_promoted(move) ((move & 0xF0000) >> 16)
#define get_move_capture(move) (move & 0x100000)
#define get_move_double(move) (move & 0x200000)
#define get_move_enpassant(move) (move & 0x400000)
#define get_move_castling(move) (move & 0x800000)

// move list struct
typedef struct{
  int moves[256];
  int count;
} moves;

// take back and copy board macros
#define take_back()                                                       \
    memcpy(bitboards, bitboards_copy, 96);                                \
    memcpy(occupancies, occupancies_copy, 24);                            \
    side = side_copy, enpassant = enpassant_copy, castle = castle_copy;   \

// preserve board state
#define copy_board()                                                      \
    U64 bitboards_copy[12], occupancies_copy[3];                          \
    int side_copy, enpassant_copy, castle_copy;                           \
    memcpy(bitboards_copy, bitboards, 96);                                \
    memcpy(occupancies_copy, occupancies, 24);                            \
    side_copy = side, enpassant_copy = enpassant, castle_copy = castle;   \

// ------------------------------------------
// function declarations
// ------------------------------------------

// board output
void print_bitboard(U64 bitboard); 
void print_board(void);
void parse_fen(char *fen);
void print_attacked(int side);
void print_move(int move);
void print_move_list(moves *move_list);

// random and magic numbers
unsigned int random_u32(void); 
U64 random_u64(void);
U64 generate_magic_num(void);
U64 find_magic_num(int square, int relevant_bits, int bishop);
void init_magic_nums(void);

// attack masks
U64 mask_pawn_attacks(int side, int square); 
U64 mask_knight_attacks(int square);
U64 mask_king_attacks(int square);
U64 mask_bishop_attacks(int square);
U64 mask_rook_attacks(int square);
U64 mask_bishop_attack_with_block(int square, U64 block);
U64 mask_rook_attack_with_block(int square, U64 block);
U64 set_occup(int index, int bits_in_mask, U64 attack_mask);

// initalizations
void init_all(void); 
void init_leapers_attacks(void);
void init_sliders_attacks(int bishop);

// homeless
unsigned long get_time_ms(void);
void perft_test(int depth);

/* STATIC INLINE FUNCTION LIST
int count_bits(U64 bitboard);
int get_lsb_index(U64 bitboard);
U64 get_rook_attacks(int square, U64 occupancy);
U64 get_bishop_attacks(int square, U64 occupancy);
U64 get_queen_attacks(int square, U64 occupancy);
int is_sq_attacked(int square, int side);
void generate_moves(moves *move_list);
int make_move(int move, int move_flag);
void perft_driver(int depth);
void add_move(moves * move_list, int move);
*/

// -------------------------------------------
// global board variables, constants and enumerations
// -------------------------------------------

// board squares (no_sq = no square)
enum {
    a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1, no_sq
};

enum { P, N, B, R, Q, K, p, n, b, r, q, k }; // piece encoding
enum { white, black, both };                 // side colors
enum { rook, bishop };                       // qol for bishop/rook attacks
enum { all_moves, capture_moves };           // distinguish captures from quiet moves
enum { wk = 1, wq = 2, bk = 4, bq = 8 };     // castling

// unicode pieces, use top for light mode, bottom for dark mode
//char *unicode_pieces[12] = {"♙", "♘", "♗", "♖", "♕", "♔", "♟︎", "♞", "♝", "♜", "♛", "♚"}; 
char *unicode_pieces[12] = {"♟︎", "♞", "♝", "♜", "♛", "♚", "♙", "♘", "♗", "♖", "♕", "♔"}; 
char ascii_pieces[12] = "PNBRQKpnbrqk";      // ascii Pieces

// attack initializations
U64 pawn_attacks[2][64];
U64 knight_attacks[64];
U64 king_attacks[64];
U64 bishop_masks[64];
U64 rook_masks[64];
U64 bishop_attacks[64][512]; // [square][occupancies]
U64 rook_attacks[64][4096];  // [square][occupancies]

U64 bitboards[12];     // bitboards, 6 for white pieces, 6 for black
U64 occupancies[3];    // white/black/all current occupancies
int side = -1;         // side to move 0=white, 1=black
int enpassant = no_sq; // for enpassant moves
int castle;            // hold encoded castling rights
U64 nodes;             // leaf nodes

// board constants for proper move generation
const U64 not_a_file = 18374403900871474942ULL;   
const U64 not_h_file = 9187201950435737471ULL;
const U64 not_hg_file = 4557430888798830399ULL;
const U64 not_ab_file = 18229723555195321596ULL;

// coordinates
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

// UCI promoted pieces must be lower case regardless of color, ie: e7e8q not e7e8Q
char promoted_pieces[] = {
    [Q] = 'q',
    [R] = 'r',
    [B] = 'b',
    [N] = 'n',
    [q] = 'q',
    [r] = 'r',
    [b] = 'b',
    [n] = 'n'
};

// convert ascii pieces to encoded constants
int char_pieces[] = {
  ['P'] = P,
  ['N'] = N,
  ['B'] = B,
  ['R'] = R,
  ['Q'] = Q,
  ['K'] = K,
  ['p'] = p,
  ['n'] = n,
  ['b'] = b,
  ['r'] = r,
  ['q'] = q,
  ['k'] = k
};

/* castling rights binary encoding
    bin  dec
    
   0001    1  white king can castle to the king side
   0010    2  white king can castle to the queen side
   0100    4  black king can castle to the king side
   1000    8  black king can castle to the queen side
   examples
   1111       both sides an castle both directions
   1001       black king => queen side
              white king => king side
*/

/* castling rights for square mapping
                           castling   move     in      in
                              right update     binary  decimal
 king & rooks didn't move:     1111 & 1111  =  1111    15
        white king  moved:     1111 & 1100  =  1100    12
  white king's rook moved:     1111 & 1110  =  1110    14
 white queen's rook moved:     1111 & 1101  =  1101    13
     
         black king moved:     1111 & 0011  =  1011    3
  black king's rook moved:     1111 & 1011  =  1011    11
 black queen's rook moved:     1111 & 0111  =  0111    7
*/

// castling rights update constants
const int castling_rights[64] = {
     7, 15, 15, 15,  3, 15, 15, 11,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    13, 15, 15, 15, 12, 15, 15, 14
};

// relevant bits and magic nums for bishops and rooks
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
// static inline functions
// ----------------------------------------

// count bits on the current board
static inline int count_bits(U64 bitboard){
  int bitcount = 0;

  while(bitboard){
    bitcount++;
    bitboard &= bitboard - 1;
  }

  return bitcount;
}

// get least significant bit index
static inline int get_lsb_index(U64 bitboard){
  int index;
  if(bitboard){
    index = count_bits((bitboard & - bitboard)-1);
    return index;
  } else {
    return -1;
  }  
}

// generate bishop attack table
static inline U64 get_bishop_attacks(int square, U64 occupancy){
  occupancy &= bishop_masks[square];
  occupancy *= bishop_magic_nums[square];
  occupancy >>= 64 - bishop_relevant_bits[square];

  return bishop_attacks[square][occupancy];
}

// generate rook attack table
static inline U64 get_rook_attacks(int square, U64 occupancy){
  occupancy &= rook_masks[square];
  occupancy *= rook_magic_nums[square];
  occupancy >>= 64 - rook_relevant_bits[square];

  return rook_attacks[square][occupancy];
}

// produce queen attack table via superpostion of rook & bishop
static inline U64 get_queen_attacks(int square, U64 occupancy){
  U64 queen_attacks = 0ULL;
  U64 bishop_occupancy = occupancy;
  U64 rook_occupancy = occupancy;

  // bishop attacks
  bishop_occupancy &= bishop_masks[square];
  bishop_occupancy *= bishop_magic_nums[square];
  bishop_occupancy >>= 64 - bishop_relevant_bits[square];

  queen_attacks = bishop_attacks[square][bishop_occupancy];

  // rook attacks
  rook_occupancy &= rook_masks[square];
  rook_occupancy *= rook_magic_nums[square];
  rook_occupancy >>= 64 - rook_relevant_bits[square];

  // superposition rook attacks with bishop attacks for queen
  queen_attacks |= rook_attacks[square][rook_occupancy];

  return queen_attacks;
}

// test if given square is attacked by given side
static inline int is_sq_attacked(int square, int side){

  // pawn attacks depending on color
  if((side == white) && (pawn_attacks[black][square] & bitboards[P])){
    return 1;
  }

  if((side == black) && (pawn_attacks[white][square] & bitboards[p])){
    return 1;
  }

  // knight attacks
  if(knight_attacks[square] & ((side == white) ? bitboards[N] : bitboards[n])){
    return 1;
  }

  // king attacks
  if(king_attacks[square] & ((side == white) ? bitboards[K] : bitboards[k])){
    return 1;
  }

  // bishop attacks
  if(get_bishop_attacks(square, occupancies[both]) & ((side == white) ? bitboards[B] : bitboards[b])){
    return 1;
  }

  // rook attacks
  if(get_rook_attacks(square, occupancies[both]) & ((side == white) ? bitboards[R] : bitboards[r])){
    return 1;
  }

  // queen attacks
  if(get_queen_attacks(square, occupancies[both]) & ((side == white) ? bitboards[Q] : bitboards[q])){
    return 1;
  }
  return 0;

}

// add a move to the move list
static inline void add_move(moves * move_list, int move){
  move_list->moves[move_list->count] = move;
  move_list->count++;
}

// make move on board
static inline int make_move(int move, int move_flag){

  // quiet moves (non captures)
  if(move_flag == all_moves){
    copy_board();

    // move parsing
    int source_sq = get_move_source(move);
    int target_sq = get_move_target(move);
    int piece = get_move_piece(move);
    int promotion = get_move_promoted(move);
    int capture = get_move_capture(move);
    int doub = get_move_double(move);
    int enpass = get_move_enpassant(move);
    int castling = get_move_castling(move);

    // move piece
    pop_bit(bitboards[piece], source_sq);
    set_bit(bitboards[piece], target_sq);

    // handle removing captured pieces
    if(capture){
      int start_piece, end_piece;

      // if captured piece is black we loop thru black, otherwise loop thru white
      if(side == white){
        start_piece = p;
        end_piece = k;
      } else {
        start_piece = P;
        end_piece = K;
      }

      for(int piece_idx = start_piece; piece_idx <= end_piece; piece_idx++){
        if(get_bit(bitboards[piece_idx], target_sq)){
          pop_bit(bitboards[piece_idx], target_sq);
          break;
        }
      }
    }

    // handle pawn promotion
    if(promotion){
      // remove pawn from promotion square
      pop_bit(bitboards[side == white ? P : p], target_sq);

      // insert promotion piece
      set_bit(bitboards[promotion], target_sq);
    }

    // handle removing pawns captured via enpassant
    if (enpass){
      if(side == white){
        pop_bit(bitboards[p], target_sq + 8);
      } else {
        pop_bit(bitboards[P], target_sq - 8);
      }
    }

    // reset enpassant to n/a every time a move is made
    enpassant = no_sq;

    // set new enpassant square if there is a double pawn push
    if(doub){
      if(side == white){
        enpassant = target_sq + 8;
      } else {
        enpassant = target_sq - 8;
      }
    }

    // handle moving rook during castling moves
    if(castling){
      switch(target_sq){
        case (g1):  // white kingside
          pop_bit(bitboards[R], h1);
          set_bit(bitboards[R], f1);
          break;
        case (c1):  // white queenside
          pop_bit(bitboards[R], a1);
          set_bit(bitboards[R], d1);
          break;
        case (g8): // black kingside
          pop_bit(bitboards[r], h8);
          set_bit(bitboards[r], f8);
          break;
        case (c8): // black queenside
          pop_bit(bitboards[r], a8);
          set_bit(bitboards[r], d8);
          break;
        default:
          break;
      }
    }

    // update the castling rights after the move
    castle &= castling_rights[source_sq];
    castle &= castling_rights[target_sq];

    // update occupancy boards
    memset(occupancies, 0ULL, 24);
    for(int piece_idx = P; piece_idx <= K; piece_idx++){
      occupancies[white] |= bitboards[piece_idx];
    }
    for(int piece_idx = p; piece_idx <= k; piece_idx++){
      occupancies[black] |= bitboards[piece_idx];
    }

    occupancies[both] |= occupancies[white];
    occupancies[both] |= occupancies[black];
   

    // change side to move
    side ^= 1;

    // check move for legality (king isn't moving into check)
    if(is_sq_attacked((side == white) ? get_lsb_index(bitboards[k]) : get_lsb_index(bitboards[K]), side)){
      // move is illegal because king is under fire, so undo move
      take_back();
      return 0;
    } else {
      return 1;
    }
  }
  
  // capture moves
  else {
    if(get_move_capture(move)){
      make_move(move, all_moves);
    } else {
      return 0;
    }
  }


}

// generate all possible moves for the given board position, side to move from fen
static inline void generate_moves(moves *move_list){
  int source_sq, target_sq;
  U64 bitboard, attacks;
  move_list->count = 0;

  for(int piece = P; piece <= k; piece++){
    bitboard = bitboards[piece];

    // side dependent pawn moves
    if(side == white){
     if(piece == P){
      while(bitboard){
        source_sq = get_lsb_index(bitboard);
        target_sq = source_sq - 8;

        if(!(target_sq < a8) && !get_bit(occupancies[both], target_sq)){
          // pawn promotion moves
          if(source_sq >= a7 && source_sq <= h7){
              add_move(move_list, encode_move(source_sq, target_sq, piece, Q, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, R, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, B, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, N, 0, 0, 0, 0));
          } else{
            // standard pawn push
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
            // double pawn push for 2nd rank pawns
            if((source_sq >= a2 && source_sq <= h2) && !get_bit(occupancies[both], target_sq - 8)){
              add_move(move_list, encode_move(source_sq, target_sq - 8, piece, 0, 0, 1, 0, 0));
            }
          }
        }

        // init attacks bitboard for pawns
        attacks = pawn_attacks[side][source_sq] & occupancies[black];

        // generate pawn captures
        while(attacks){
          target_sq = get_lsb_index(attacks);

          if(source_sq >= a7 && source_sq <= h7){
              add_move(move_list, encode_move(source_sq, target_sq, piece, Q, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, R, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, B, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, N, 1, 0, 0, 0));
          } else {
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
          }

          pop_bit(attacks, target_sq);
        }

        if(enpassant != no_sq){
          U64 enpassant_attacks = pawn_attacks[side][source_sq] & (1ULL << enpassant);
          if(enpassant_attacks) {
            int target_enpassant = get_lsb_index(enpassant_attacks);
            add_move(move_list, encode_move(source_sq, target_enpassant, piece, 0, 1, 0, 1, 0));
          }
        }

        pop_bit(bitboard, source_sq);
      }
     }

      // castling moves
      if(piece == K){
        if(castle & wk){
          if(!get_bit(occupancies[both],f1) && !get_bit(occupancies[both],g1)){
            if(!is_sq_attacked(f1, black) && !is_sq_attacked(e1, black)){
              add_move(move_list, encode_move(e1, g1, piece, 0, 0, 0, 0, 1));
            }
          }
        }
        if(castle & wq){
          if(!get_bit(occupancies[both],d1) && !get_bit(occupancies[both],c1) && !get_bit(occupancies[both],b1)){
            if(!is_sq_attacked(d1, black) && !is_sq_attacked(e1, black)){
              add_move(move_list, encode_move(e1, c1, piece, 0, 0, 0, 0, 1));
            }
          }
        }
      }
    } else {
      if(piece == p){
        while(bitboard){
          source_sq = get_lsb_index(bitboard);
          target_sq = source_sq + 8;

          if(!(target_sq > h1) && !get_bit(occupancies[both], target_sq)){
            // pawn promotion moves
            if(source_sq >= a2 && source_sq <= h2){
              add_move(move_list, encode_move(source_sq, target_sq, piece, q, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, r, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, b, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, n, 0, 0, 0, 0));
            } else {
              // standard pawn push
              add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
              // double pawn push for 7th rank pawns
              if((source_sq >= a7 && source_sq <= h7) && !get_bit(occupancies[both], target_sq + 8)){
                add_move(move_list, encode_move(source_sq, target_sq + 8, piece, 0, 0, 1, 0, 0));
              }
            }
          }

          // init attacks bitboard for pawns
          attacks = pawn_attacks[side][source_sq] & occupancies[white];

          // generate pawn captures
          while(attacks){
            target_sq = get_lsb_index(attacks);

            if(source_sq >= a2 && source_sq <= h2){
              add_move(move_list, encode_move(source_sq, target_sq, piece, q, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, r, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, b, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_sq, target_sq, piece, n, 1, 0, 0, 0));
            } else {
              add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
            }

            pop_bit(attacks, target_sq);
          }

          if(enpassant != no_sq){
            U64 enpassant_attacks = pawn_attacks[side][source_sq] & (1ULL << enpassant);
            if(enpassant_attacks) {
              int target_enpassant = get_lsb_index(enpassant_attacks);
              add_move(move_list, encode_move(source_sq, target_enpassant, piece, 0, 1, 0, 1, 0));
            }
          }

          pop_bit(bitboard, source_sq);
        }
      }
      // castling moves
      if(piece == k){
        if(castle & bk){
          if(!get_bit(occupancies[both],f8) && !get_bit(occupancies[both],g8)){
            if(!is_sq_attacked(e8, white) && !is_sq_attacked(f8, white)){
              add_move(move_list, encode_move(e8, g8, piece, 0, 0, 0, 0, 1));
            }
          }
        }
        if(castle & bq){
          if(!get_bit(occupancies[both],d8) && !get_bit(occupancies[both],c8) && !get_bit(occupancies[both],b8)){
            if(!is_sq_attacked(d8, white) && !is_sq_attacked(e8, white) ){
              add_move(move_list, encode_move(e8, c8, piece, 0, 0, 0, 0, 1));
            }
          }
        }
      }

    }
  
    if((side == white) ? piece == N : piece == n){
      while(bitboard){
        source_sq = get_lsb_index(bitboard);
        attacks = knight_attacks[source_sq] & ((side == white) ? ~occupancies[white] : ~occupancies[black]);
        while(attacks){
          target_sq = get_lsb_index(attacks);

          if(!get_bit(((side == white) ? occupancies[black] : occupancies[white]), target_sq)){
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
          } else {
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
          }

          pop_bit(attacks, target_sq);
        }

        pop_bit(bitboard, source_sq);
      }
    }

    if((side == white) ? piece == B : piece == b){
      while(bitboard){
        source_sq = get_lsb_index(bitboard);
        attacks = get_bishop_attacks(source_sq, occupancies[both]) & ((side == white) ? ~occupancies[white] : ~occupancies[black]);
        while(attacks){
          target_sq = get_lsb_index(attacks);

          if(!get_bit(((side == white) ? occupancies[black] : occupancies[white]), target_sq)){
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
          } else {
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
          }

          pop_bit(attacks, target_sq);
        }

        pop_bit(bitboard, source_sq);
      }
    }

    if((side == white) ? piece == R : piece == r){
      while(bitboard){
        source_sq = get_lsb_index(bitboard);
        attacks = get_rook_attacks(source_sq, occupancies[both]) & ((side == white) ? ~occupancies[white] : ~occupancies[black]);
        while(attacks){
          target_sq = get_lsb_index(attacks);

          if(!get_bit(((side == white) ? occupancies[black] : occupancies[white]), target_sq)){
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
          } else {
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
          }

          pop_bit(attacks, target_sq);
        }

        pop_bit(bitboard, source_sq);
      }
    }

    if((side == white) ? piece == Q : piece == q){
      while(bitboard){
        source_sq = get_lsb_index(bitboard);
        attacks = get_queen_attacks(source_sq, occupancies[both]) & ((side == white) ? ~occupancies[white] : ~occupancies[black]);
        while(attacks){
          target_sq = get_lsb_index(attacks);

          if(!get_bit(((side == white) ? occupancies[black] : occupancies[white]), target_sq)){
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
          } else {
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
          }

          pop_bit(attacks, target_sq);
        }

        pop_bit(bitboard, source_sq);
      }
    }

    if((side == white) ? piece == K : piece == k){
      while(bitboard){
        source_sq = get_lsb_index(bitboard);
        attacks = king_attacks[source_sq] & ((side == white) ? ~occupancies[white] : ~occupancies[black]);
        while(attacks){
          target_sq = get_lsb_index(attacks);

          if(!get_bit(((side == white) ? occupancies[black] : occupancies[white]), target_sq)){
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 0, 0, 0, 0));
          } else {
            add_move(move_list, encode_move(source_sq, target_sq, piece, 0, 1, 0, 0, 0));
          }

          pop_bit(attacks, target_sq);
        }

        pop_bit(bitboard, source_sq);
      }
    }
  }
}

// performance testing to ensure board representation fidelity
static inline void perft_driver(int depth){
  // recursion escape condition
  if(depth == 0){
    nodes++;
    return;
  }

  moves move_list[1];
  generate_moves(move_list);

  for(int i = 0; i < move_list->count; i++){
    copy_board();
    if(!make_move(move_list->moves[i], all_moves)){
      continue;
    }
    //print_move(move_list->moves[i]);
    perft_driver(depth-1);
    take_back();
  }
}

// ------------------------------------------
// board output
// ------------------------------------------

// print board and board number
void print_bitboard(U64 bitboard){

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

// print current board state
void print_board(){
  printf("\n");
  for (int rank = 0; rank < 8; rank++){
    for (int file = 0; file < 8; file++){
      int square = rank * 8 + file;
      int piece = -1;

      // print rank number layout
      if(!file){
        printf("  %d ", 8 - rank);
      }

      // loop over piece bitboards to find what piece resides on this square
      for (int bb_idx = P; bb_idx <= k; bb_idx++){
        if(get_bit((bitboards[bb_idx]), square)){
          piece = bb_idx;
        }
      }

      // had to do some crazy unicode spacing to make the board look alright in my terminal
      printf(" %s", (piece == -1) ? "·" : unicode_pieces[piece]);


    }
    printf("\n");
  }
  // print file layout
  printf("\n     a b c d e f g h\n");
  printf("\nSIDE TO MOVE:          %s", !side ? "White" : "Black");
  printf("\nAVAILABLE ENPASSANT:   %s", enpassant != no_sq ? square_to_coord[enpassant] : "N/A");
  printf("\nWHITE CASTLING RIGHTS: %s, %s", (castle & wk) ? "Kingside" : "--------", (castle & wq) ? "Queenside" : "--------");
  printf("\nBLACK CASTLING RIGHTS: %s, %s", (castle & bk) ? "Kingside" : "--------", (castle & bq) ? "Queenside" : "--------");
  printf("\n\n");
}

// FEN Parsing
void parse_fen(char *fen){
  
  // reset bitboards and state
  memset(bitboards, 0ULL, sizeof(bitboards));
  memset(occupancies, 0ULL, sizeof(occupancies));
  side = 0;
  enpassant = no_sq;
  castle = 0;

  // loop over board to get board state
  for(int rank = 0; rank < 8; rank++){
    for(int file = 0; file < 8; file++){
      int square = rank * 8 + file;
      
      if((*fen >= 'a' && *fen <= 'z') || (*fen >= 'A' && *fen < 'Z')){
        int piece = char_pieces[*fen];
        set_bit(bitboards[piece], square);
        fen++;
      }

      if (*fen >= '0' && *fen <= '9'){
        // little trick here to convert char 0 to int 0
        int offset = *fen - '0';
        int piece = -1;

        // loop over piece bitboards to find what piece resides on this square
        for (int bb_idx = P; bb_idx <= k; bb_idx++){
          if(get_bit((bitboards[bb_idx]), square)){
            piece = bb_idx;
          }
        }

        if(piece == -1){
          file--;
        }

        file += offset;
        fen++;
        }

      if(*fen == '/'){
        fen++;
      }
    }
  }

  fen++;

  // parse side to move
  (*fen == 'w') ? (side = white) : (side = black);

  // parse castling rights
  fen += 2; 
  while(*fen != ' '){
    switch(*fen){
      case 'K': castle |= wk; break;
      case 'Q': castle |= wq; break;
      case 'k': castle |= bk; break;
      case 'q': castle |= bq; break;
    }
    fen++;
  }

  // parse enpassant square
  *fen++;
  if(*fen != '-'){
    int enpass_file = fen[0] - 'a';
    int enpass_rank = 8 - (fen[1] - '0');
    enpassant = enpass_rank * 8 + enpass_file;
  } else {
    enpassant = no_sq;
  }

  // initialization of occupancy bitboards

  // white occupancy bitboards
  for(int piece = P; piece <= K; piece++){
    occupancies[white] |= bitboards[piece];
  }

  // black occupancy bitboards
  for(int piece = p; piece <= k; piece++){
    occupancies[black] |= bitboards[piece];
  }

  occupancies[both] = occupancies[white] | occupancies[black];
}

// print attacked pieces by given side
void print_attacked(int side){
  for (int rank=0; rank<8; rank++){
    for(int file=0; file<8; file++){
      int square = rank*8 + file;

      // print rank number layout
      if(!file){
        printf("  %d ", 8 - rank);
      }

      printf("  %d", is_sq_attacked(square, side) ? 1 : 0);
    }
    printf("\n");
  }

  // print file layout
  printf("\n      a  b  c  d  e  f  g  h\n\n");
}

// UCI print move
void print_move(int move){
  printf("%s%s%c\n", square_to_coord[get_move_source(move)], square_to_coord[get_move_target(move)], promoted_pieces[get_move_promoted(move)]);
}

// print move list with information
void print_move_list(moves *move_list){
  printf("%d", move_list->count);
  printf("-------------------------------------------\n");
  printf("%-8s%-8s%-8s%-8s%-8s%-8s\n", "MOVE", "PIECE", "CAPTURE", "DOUBLE", "ENPASS", "CASTLE");
  printf("-----------------------------------------------\n");

  for(int move_count = 0; move_count < move_list->count; move_count++){
    int move = move_list->moves[move_count];
    printf("%s%-2s%-4c%-8c%-8d%-8d%-8d%-8d\n",square_to_coord[get_move_source(move)], 
                                        square_to_coord[get_move_target(move)], 
                                        get_move_promoted(move) ? promoted_pieces[get_move_promoted(move)] : ' ',
                                        ascii_pieces[get_move_piece(move)],
                                        get_move_capture(move) ? 1 : 0,
                                        get_move_double(move) ? 1 : 0,
                                        get_move_enpassant(move) ? 1 : 0,
                                        get_move_castling(move) ? 1 : 0
                                        );
  }
}

// -------------------------------------------
// random and magic numbers
// -------------------------------------------
unsigned int state = 1804289383; // seed 

// produces a random unsigned int
unsigned int random_u32(){
  unsigned int x = state;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  state = x;
  return x;
} 

// produce a random U64
U64 random_u64(){
  U64 n1, n2, n3, n4;

  // init random numbers slicing 16 bit from MSB side (MSB side is bottom up)
  n1 = (U64)(random_u32()) & 0xFFFF;
  n2 = (U64)(random_u32()) & 0xFFFF;
  n3 = (U64)(random_u32()) & 0xFFFF;
  n4 = (U64)(random_u32()) & 0xFFFF; 

  return n1 | (n2 << 16) | (n3 << 32) | (n4 << 48);
}

// create magic numbers for the rook and bishop
U64 generate_magic_num(){
  return random_u64() & random_u64() & random_u64();
}

// locate magic numbers for bishop or rook
U64 find_magic_num(int square, int relevant_bits, int bishop){
  U64 occupancies[4096];
  U64 attacks[4096];
  U64 used_attacks[4096];
  U64 attack_mask = bishop ? mask_bishop_attacks(square) : mask_rook_attacks(square);

  int occupancy_indices = 1 << relevant_bits;
  for(int idx = 0; idx < occupancy_indices; idx++){
    occupancies[idx] = set_occup(idx, relevant_bits, attack_mask);
    attacks[idx] = bishop ? mask_bishop_attack_with_block(square, occupancies[idx]) : mask_rook_attack_with_block(square, occupancies[idx]); 
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

// initialize magic numbers
void init_magic_nums(){
  for(int square = 0; square < 64; square++){
    printf(" 0x%llxULL,\n", find_magic_num(square, rook_relevant_bits[square], rook));
    printf(" 0x%llxULL,\n", find_magic_num(square, bishop_relevant_bits[square], bishop));
  }
}

// ------------------------------------------
// attack masks
// ------------------------------------------

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

// bishop attack mask generation (without edges)
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

// rook attack mask generation (without edges)
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

// bishop attack mask with blocked paths accounted for
U64 mask_bishop_attack_with_block(int square, U64 block){
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

// rook attack mask with blocked paths accounted for
U64 mask_rook_attack_with_block(int square, U64 block){
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

// -------------------------------------------
// FUNCTIONS TO BE MOVED TO PROPER HOME IN FUTURE
// -------------------------------------------

unsigned long get_time_ms(){
  return GetTickCount();
}

void perft_test(int depth){
  moves move_list[1];
  generate_moves(move_list);

  unsigned long start = get_time_ms();
  for(int i = 0; i < move_list->count; i++){
    copy_board();
    if(!make_move(move_list->moves[i], all_moves)){
      continue;
    }

    unsigned long long node_sum = nodes;

    perft_driver(depth-1);
    unsigned long long prev_nodes = nodes - node_sum;
    take_back();


    printf("Move: %s%s%c", square_to_coord[get_move_source(move_list->moves[i])], 
                      square_to_coord[get_move_target(move_list->moves[i])], 
                      promoted_pieces[get_move_promoted(move_list->moves[i])]);
    printf("   Nodes: %llu\n", prev_nodes);
  }
  unsigned long total_time = get_time_ms() - start;

  printf("\n    Depth: %d\n    Nodes: %llu\n     Time: %ld ms\n ", depth, nodes, total_time);
}

// ----------------------------------------
// intializations
// ----------------------------------------

// initialize everything
void init_all(){
  init_leapers_attacks();
  init_sliders_attacks(bishop);
  init_sliders_attacks(rook);
}

// initialize all leaper pieces (pawn, knight, king)
void init_leapers_attacks(){
  for (int square=0; square<64; square++){
    pawn_attacks[white][square] = mask_pawn_attacks(white, square);
    pawn_attacks[black][square] = mask_pawn_attacks(black, square);
    knight_attacks[square] = mask_knight_attacks(square);
    king_attacks[square] = mask_king_attacks(square);
  }
}

// initialize all slider pieces (bishop, rook)
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

        bishop_attacks[square][magic_idx] = mask_bishop_attack_with_block(square, occupancy);
      } else {
        // ROOK
        U64 occupancy = set_occup(idx, relevant_bits, attack_mask);
        int magic_idx = (occupancy * rook_magic_nums[square]) >> (64 - rook_relevant_bits[square]);

        rook_attacks[square][magic_idx] = mask_rook_attack_with_block(square, occupancy);
      }
    }
  }
}

// ----------------------------------------
// main 
// ----------------------------------------

int main(){

  init_all();
  parse_fen(tricky_position);
  
  //parse_fen(start_position);
      //parse_fen("r3k2r/p1p1qpb1/bn1ppnp1/1B1PN3/1p2P3/2N1Q2p/PPPBBPPP/R3K2R b KQkq - 0 1 ");
  print_board();


  perft_test(4);

  
  return 0;
}