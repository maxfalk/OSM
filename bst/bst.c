/**
 * Binary search tree implementation
 *
 * Copyright (c) 2013 the authors listed at the following URL, and/or
 * the authors of referenced articles or incorporated external code:
 * http://en.literateprograms.org/Binary_search_tree_(C)?action=history&offset=20121127201818
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Retrieved from: http://en.literateprograms.org/Binary_search_tree_(C)?oldid=18734
 * Modified: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 */


/***********************************************************/
/* NOTE: You can modify/add any piece of code that will    */
/* make your algorithm work                                */
/***********************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "bst.h"

pthread_spinlock_t lock_cg;
pthread_spinlock_t entry_lock;


/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           the node containg the data
 */
struct bst_node**
search(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: For the Step 2 you will have to make this function thread-safe */

    struct bst_node** node = root;

    while (*node != NULL) {
        int compare_result = compare(data, (*node)->data);

        if (compare_result < 0) {
            node = &(*node)->left;
        } else if (compare_result > 0) {
            node = &(*node)->right;
        } else {
            break;
        }
    }

    return node;
}


/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void
node_delete_aux(struct bst_node** node)
{
    /* TODO: For Step 2 you will have to make this function thread-safe */

    struct bst_node* old_node = *node;

    if ((*node)->left == NULL) {
        *node = (*node)->right; // Node is replaced by its RIGHT node
        free_node(old_node);
    } else if ((*node)->right == NULL) {
        *node = (*node)->left; // Node is replaced by its LEFT node
        free_node(old_node);
    } else {

        // Here we need to lock the node we are looking at AND its parent

        struct bst_node** pred = &(*node)->left; // pred is set to the LEFT node
	
        while ((*pred)->right != NULL) { // Find the most RIGHT node of pred
	    pred = &(*pred)->right;
	}

	/* Swap values */
	void* temp = (*pred)->data; // Set the value of NODE = NODE->LEFT->(most RIGHT node value) 
	(*pred)->data = (*node)->data; 
	(*node)->data = temp;

	node_delete_aux(pred); // Remove NODE->LEFT->(most RIGHT node) instead of NODE
    }
}


/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           1 if data is not found, 0 otherwise
 */
struct node_pair *
search_fg(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = root;
    struct bst_node** parent = NULL;
    struct node_pair* pair = malloc(sizeof(struct node_pair));
    pair->parent = NULL;
    pair->child = NULL;

    while (*node != NULL) { // Unsafe area
        pthread_spin_lock(&(*node)->lock); // lock the current node

        int compare_result = compare(data, (*node)->data);

        if (compare_result < 0) {
            if (parent != NULL)
                pthread_spin_unlock(&(*parent)->lock); // unlock the current node's parent
            parent = &(*node); // Parent is now the same as the current node (and locked)
            node = &(*node)->left; // But here the current node is set to its left child
        } else if (compare_result > 0) {
            if (parent != NULL)
                pthread_spin_unlock(&(*parent)->lock); // unlock the current node's parent
            parent = &(*node); // Parent is now the same as the current node (and locked)
            node = &(*node)->right; // But here the current node is set to its right child
        } else {
            break; // Here both the current node and its parent is locked
        }
    }

    if (*node == NULL) { // No node was found
        if (*parent != NULL)
            pthread_spin_unlock(&(*parent)->lock); // unlock the current node's parent
    } else {
        pair->parent = &(*parent); // Parent is locked (unless NULL)
        pair->child = &(*node); // Child is locked
    }

    return pair;
}


/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void
node_delete_aux_fg(struct node_pair* pair)
{
    struct bst_node** parent = (pair->parent != NULL) ? &(*(pair->parent)) : NULL; // parent is locked (unless NULL)
    struct bst_node** node = &(*(pair->child)); // node is locked
    struct bst_node* old_node = *node; // old_node is locked


    if ((*node)->left == NULL) { 
        if ((*node)->right != NULL) 
            pthread_spin_lock(&(*node)->right->lock);
        *node = (*node)->right; 
        free_node(old_node); // old_node's lock is destroyed, but node is still locked unless NULL
    } else if ((*node)->right == NULL) { 
        pthread_spin_lock(&(*node)->left->lock); 
        *node = (*node)->left; 
        free_node(old_node); // old_node's lock is destroyed, but node is still locked
    } else {         
        pthread_spin_lock(&(*node)->left->lock); 
        struct bst_node** pred_parent = NULL;
        struct bst_node** pred = &(*node)->left; 

        while ((*pred)->right != NULL) {
            if (pred_parent != NULL)
                pthread_spin_unlock(&(*pred_parent)->lock); // Unlock pred_parent
            pthread_spin_lock(&(*pred)->right->lock); // Lock pred->right
            pred_parent = &(*pred);
	    pred = &(*pred)->right;
	}

        // pred is either the right most node or node->left
        // pred_parent is either NULL or parent of right most

	/* Swap values */
	void* temp = (*pred)->data; 
	(*pred)->data = (*node)->data; 
	(*node)->data = temp;

        struct node_pair* pred_pair = malloc(sizeof(struct node_pair));
        pred_pair->parent = &(*pred_parent);
        pred_pair->child = &(*pred);

	node_delete_aux_fg(pred_pair);

        free(pred_pair);
    }

    if (node != NULL && *node != NULL) 
        pthread_spin_unlock(&(*node)->lock); // Unlock node

    if (parent != NULL && *parent != NULL) 
        pthread_spin_unlock(&(*parent)->lock); // Unlock parent
}


/**
 * Deletes the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);

    if (*node == NULL) {
        return -1;
    }

    node_delete_aux(node);

    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_cg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */

    pthread_spin_lock(&lock_cg);

    struct bst_node** node = search(root, compare, data);

    if (node == NULL) {
        pthread_spin_unlock(&lock_cg);
        return -1;
    }
    
    node_delete_aux(node);

    pthread_spin_unlock(&lock_cg);

    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_fg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */
    struct node_pair* pair = search_fg(root, compare, data);

    if (pair->child == NULL)
        return -1;

    node_delete_aux_fg(pair);

    free(pair);
    return 0;
}


/**
 * Allocate resources and initialize a BST.
 *
 * @return           root of the BST
 */
struct bst_node **
tree_init(void)
{
    struct bst_node** root = malloc(sizeof(*root));
    if (root == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    *root = NULL;

    /* TODO: Initialize any global variables you use for the BST */
    pthread_spin_init(&lock_cg, 0);
    pthread_spin_init(&entry_lock, 0);
    
    return root;
}

/**
 * Remove resources for the tree.
 *
 * @param root       root of the tree
 */
void
tree_fini(struct bst_node ** root)
{
    /* TODO: Free any global variables you used for the BST */
    pthread_spin_destroy(&lock_cg);

    if (root != NULL)
        free(root);
}


/**
 * Inserts a new node with the requested data if not already in the tree.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be inserted
 * @return           1 if data is in the BST already, 0 otherwise
 */
int
node_insert(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);
    if (*node == NULL) {
        *node = new_node(data);
        return 0;
    } else
        return 1;
}


/**
 * Creates a new node with the requested data.
 *
 * @param data       pointer to the data pointed be the new node
 */
struct bst_node* 
new_node(void* data)
{
    struct bst_node* node = malloc(sizeof(struct bst_node));
    if (node == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    } else {
        /* TODO: Initialize any per node variables you use for the BST */
        
        node->left = NULL;
        node->right = NULL;
        node->data = data;
        pthread_spin_init(&(node->lock), 0);
    }

    return node;
}


/**
 * Deletes a node.
 *
 * @param node       node to be freed
 */
void
free_node(struct bst_node* node) 
{
    if (node == NULL)
        fprintf(stderr, "Invalid node\n");
    else {
        /* TODO: Finalize any per node variables you use for the BST */
        free(node);
    }
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * c-file-style: "stroustrup"
 * End:
 */
